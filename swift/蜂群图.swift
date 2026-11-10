import AppKit

struct ChartData: Decodable {
    struct Team: Decodable {
        let team: String
        let strength: Double
    }

    struct Point: Decodable {
        let value: Double
        let logo: String?
    }

    struct SubtitlePart: Decodable {
        enum Style: String, Decodable { case mono, credit }

        let label: String
        let value: String?
        let style: Style?
    }

    enum Subtitle: Decodable {
        case text(String), parts([SubtitlePart])

        init(from decoder: Decoder) throws {
            let value = try decoder.singleValueContainer()
            if let text = try? value.decode(String.self) {
                self = .text(text)
            } else {
                self = .parts(try value.decode([SubtitlePart].self))
            }
        }
    }

    struct AxisRange: Decodable {
        let min: Double
        let max: Double
    }

    let title: String
    let titlePrefix: String?
    let method: String?
    let credit: String?
    let source: String?
    let asOf: String?
    let subtitle: Subtitle?
    let xRange: AxisRange?
    let leftLabel: String?
    let rightLabel: String?
    let teams: [Team]?
    let points: [Point]?
}

func fail(_ message: String) -> Never {
    FileHandle.standardError.write(Data((message + "\n").utf8))
    exit(1)
}

let arguments = CommandLine.arguments
guard (2...3).contains(arguments.count) else {
    let script = URL(fileURLWithPath: arguments[0]).lastPathComponent
    fail("用法：swift \(script) /数据.json [/输出.png]")
}

let root = URL(fileURLWithPath: #filePath).standardizedFileURL.deletingLastPathComponent().deletingLastPathComponent()
let input = URL(fileURLWithPath: arguments[1]).standardizedFileURL
let output = arguments.count == 3
    ? URL(fileURLWithPath: arguments[2]).standardizedFileURL
    : root.appendingPathComponent("成品图/蜂群图.png")
guard output.pathExtension.lowercased() == "png",
      output.resolvingSymlinksInPath() != input.resolvingSymlinksInPath() else {
    fail("请指定独立的 PNG 输出路径")
}

let data: ChartData
do {
    data = try JSONDecoder().decode(ChartData.self, from: Data(contentsOf: input))
} catch {
    fail("无法读取制图数据：\(error)")
}

let logoDirectory = root.appendingPathComponent("素材/LOGO", isDirectory: true)
let rawPoints: [ChartData.Point]
if let points = data.points {
    rawPoints = points
} else {
    let teams = data.teams ?? []
    let codes = teams.map { $0.team.uppercased() }
    let invalid = CharacterSet(charactersIn: "ABCDEFGHIJKLMNOPQRSTUVWXYZ").inverted
    guard Set(codes).count == codes.count,
          codes.allSatisfy({ $0.count == 3 && $0.rangeOfCharacter(from: invalid) == nil }) else {
        fail("球队缩写须为三个英文字母且不能重复")
    }
    rawPoints = zip(teams, codes).map { team, code in
        .init(value: team.strength, logo: logoDirectory.appendingPathComponent("\(code).webp").path)
    }
}
guard (1...30).contains(rawPoints.count), rawPoints.allSatisfy({ $0.value.isFinite }) else {
    fail("请提供 1 至 30 个有效数据点")
}
let points = rawPoints.sorted { $0.value < $1.value }
let canvas = CGRect(x: 0, y: 0, width: 864, height: 648)
let panel = CGRect(x: 28, y: 106, width: canvas.width - 56, height: 500)
let markerArea = panel.insetBy(dx: 0, dy: 32)
let scale = 200.0 / 72
let preferredDiameter = points.contains(where: { $0.logo != nil }) ? 0.68 * 72 : 13

let low = points.first!.value
let high = points.last!.value
let padding = high > low ? (high - low) * 0.12 : max(1, abs(low) * 0.05)
let xMin = data.xRange?.min ?? low - padding
let xMax = data.xRange?.max ?? high + padding
let span = xMax - xMin
guard span.isFinite, span > 0, xMin <= low, high <= xMax else {
    fail("横轴范围需要覆盖所有评分，且最大值必须大于最小值")
}

func x(_ strength: Double) -> Double {
    panel.minX + panel.width * ((strength - xMin) / span)
}

func swarmCenters(diameter: Double) -> [CGPoint]? {
    let step = diameter / 12
    let steps = Int((markerArea.height - diameter) / (2 * step))
    let offsets = [0.0] + (1...steps).flatMap { [Double($0) * step, -Double($0) * step] }
    var centers: [CGPoint] = []
    for point in points {
        let centerX = x(point.value)
        guard let offset = offsets.first(where: { offset in
            centers.allSatisfy { other in
                let dx = centerX - other.x
                let dy = markerArea.midY - offset - other.y
                return dx * dx + dy * dy >= diameter * diameter - 1e-8
            }
        }) else { return nil }
        centers.append(CGPoint(x: centerX, y: markerArea.midY - offset))
    }
    return centers
}

// Preserve the reference size whenever possible. Dense ties only change the marker size.
var diameter = preferredDiameter
var layout = swarmCenters(diameter: diameter)
while layout == nil && diameter > preferredDiameter * 0.3 {
    diameter *= 0.85
    layout = swarmCenters(diameter: diameter)
}
guard let centers = layout else { fail("当前评分分布无法放进绘图区") }

let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: 2400, pixelsHigh: 1800,
                              bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
                              isPlanar: false, colorSpaceName: .deviceRGB,
                              bytesPerRow: 0, bitsPerPixel: 0)!
let context = NSGraphicsContext(bitmapImageRep: bitmap)!.cgContext
context.translateBy(x: 0, y: Double(bitmap.pixelsHigh))
context.scaleBy(x: scale, y: -scale)
context.interpolationQuality = .high
NSGraphicsContext.current = NSGraphicsContext(cgContext: context, flipped: true)

func color(_ hex: UInt32) -> NSColor {
    NSColor(srgbRed: Double((hex >> 16) & 255) / 255,
            green: Double((hex >> 8) & 255) / 255,
            blue: Double(hex & 255) / 255, alpha: 1)
}

func run(_ text: String, _ size: Double, _ color: NSColor, mono: Bool = false,
         bold: Bool = false) -> NSAttributedString {
    let name = mono ? (bold ? "Menlo-Bold" : "Menlo-Regular")
        : (bold ? "PingFangSC-Semibold" : "PingFangSC-Regular")
    let font = NSFont(name: name, size: size) ?? NSFont.systemFont(ofSize: size, weight: bold ? .semibold : .regular)
    return NSAttributedString(string: text, attributes: [.font: font, .foregroundColor: color])
}

func drawHeading(_ heading: NSAttributedString, at y: Double) {
    let width = heading.size().width
    guard width <= panel.width, !heading.string.contains(where: \.isNewline) else {
        fail("标题或副标题超出单行宽度，请缩短文字")
    }
    heading.draw(at: CGPoint(x: (canvas.width - width) / 2, y: y))
}

context.setFillColor(red: 244.0 / 255, green: 246.0 / 255, blue: 248.0 / 255, alpha: 1)
context.fill(canvas)
let ink = color(0x3a3a3a)
let styles = try JSONSerialization.jsonObject(with: Data(contentsOf: root.appendingPathComponent("样式.json")))
    as? [String: [String: Any]] ?? [:]
let heading = (styles["header"] ?? [:]).merging(styles["蜂群图"] ?? [:]) { _, value in value }
guard let titleSize = heading["title"] as? Double, let subtitleSize = heading["subtitle"] as? Double,
      let titleBold = heading["titleBold"] as? Bool, let subtitleBold = heading["subtitleBold"] as? Bool,
      let titleHex = UInt32((heading["titleColor"] as? String ?? "").dropFirst(), radix: 16),
      let subtitleHex = UInt32((heading["subtitleColor"] as? String ?? "").dropFirst(), radix: 16) else {
    fail("样式.json 缺少有效的标题样式")
}
let subtitleColor = color(subtitleHex)
let title = NSMutableAttributedString(attributedString:
    run(data.titlePrefix ?? "", titleSize, color(titleHex), mono: true, bold: titleBold))
title.append(run(data.title, titleSize, color(titleHex), bold: titleBold))
drawHeading(title, at: 28)

let subtitle = NSMutableAttributedString(string: "")
let subtitleParts: [ChartData.SubtitlePart]
switch data.subtitle {
case .text(let text): subtitleParts = [.init(label: "", value: text, style: nil)]
case .parts(let parts): subtitleParts = parts
case nil:
    subtitleParts = [
        .init(label: "", value: data.method, style: nil),
        .init(label: "制图: ", value: data.credit, style: .credit),
        .init(label: "数据源: ", value: data.source, style: .mono),
        .init(label: "截至 ", value: data.asOf, style: .mono),
    ]
}
let subtitleSeparator = data.subtitle == nil ? " | " : "｜"
for part in subtitleParts {
    guard let value = part.value, !value.isEmpty else { continue }
    let separator = subtitle.length == 0 ? "" : subtitleSeparator
    subtitle.append(run(separator + part.label, subtitleSize, subtitleColor, bold: subtitleBold))
    let isCredit = part.style == .credit
    subtitle.append(run(value, isCredit ? subtitleSize * 1.44 : subtitleSize, isCredit ? .black : subtitleColor,
                        mono: part.style == .mono, bold: isCredit || subtitleBold))
}
drawHeading(subtitle, at: 66)

let rawStep = span / 8.25
let magnitude = pow(10, floor(log10(rawStep)))
let fraction = rawStep / magnitude
let tickStep = (fraction <= 1 ? 1.0 : fraction <= 2 ? 2.0 : fraction <= 5 ? 5.0 : 10.0) * magnitude
guard tickStep.isFinite, tickStep > 0 else { fail("横轴范围无法生成刻度") }
let gridColor = color(0xe7e4df)
let zeroColor = color(0xb9b5ae)
let tickColor = color(0x6a6865)
for index in stride(from: ceil(xMin / tickStep), through: floor(xMax / tickStep), by: 1) {
    let tick = index * tickStep
    let tickX = x(tick)
    context.saveGState()
    context.setStrokeColor((tick == 0 ? zeroColor : gridColor).cgColor)
    context.setLineWidth(tick == 0 ? 0.8 : 0.55)
    if tick == 0 { context.setLineDash(phase: 0, lengths: [4, 3]) }
    context.move(to: CGPoint(x: tickX, y: panel.minY))
    context.addLine(to: CGPoint(x: tickX, y: panel.maxY))
    context.strokePath()
    context.restoreGState()
    let number = String(format: "%g", tick)
    let label = run(data.points == nil && tick > 0 ? "+" + number : number, 13, tickColor, mono: true)
    label.draw(at: CGPoint(x: tickX - label.size().width / 2, y: panel.maxY + 4))
}

let logoSize = diameter * 0.8
for (point, center) in zip(points, centers) {
    guard let path = point.logo else {
        context.setFillColor(ink.cgColor)
        context.fillEllipse(in: CGRect(x: center.x - 5, y: center.y - 5, width: 10, height: 10))
        continue
    }
    let file = URL(fileURLWithPath: path, relativeTo: input.deletingLastPathComponent())
    guard let logo = NSImage(contentsOf: file), logo.isValid, max(logo.size.width, logo.size.height) > 0 else {
        fail("无法读取 Logo：\(file.path)")
    }
    let factor = logoSize / max(logo.size.width, logo.size.height)
    let width = logo.size.width * factor, height = logo.size.height * factor
    let rect = CGRect(x: center.x - width / 2, y: center.y - height / 2, width: width, height: height)
    logo.draw(in: rect, from: .zero, operation: .sourceOver, fraction: 1, respectFlipped: true,
              hints: [.interpolation: NSImageInterpolation.high])
}

if let left = data.leftLabel, !left.isEmpty {
    run("← \(left)", 15.6, ink, bold: true).draw(at: CGPoint(x: panel.minX + 7, y: panel.maxY - 27))
}
if let right = data.rightLabel, !right.isEmpty {
    let label = run("\(right) →", 15.6, ink, bold: true)
    label.draw(at: CGPoint(x: panel.maxX - 7 - label.size().width, y: panel.maxY - 27))
}

bitmap.size = canvas.size
guard let png = bitmap.representation(using: .png, properties: [:]) else { fail("无法编码 PNG") }
do {
    try FileManager.default.createDirectory(at: output.deletingLastPathComponent(), withIntermediateDirectories: true)
    try png.write(to: output, options: .atomic)
} catch {
    fail("无法保存图表：\(error.localizedDescription)")
}
print("已保存 \(points.count) 个数据点的图表：\(output.path)")
