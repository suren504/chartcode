import AppKit

struct ChartData: Decodable {
    struct Team: Decodable {
        let team: String
        let strength: Double
    }

    struct SubtitlePart: Decodable {
        enum Style: String, Decodable { case mono, credit }

        let label: String
        let value: String?
        let style: Style?
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
    let subtitle: [SubtitlePart]?
    let xRange: AxisRange?
    let leftLabel: String?
    let rightLabel: String?
    let teams: [Team]
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

let input = URL(fileURLWithPath: arguments[1]).standardizedFileURL
let output = arguments.count == 3
    ? URL(fileURLWithPath: arguments[2]).standardizedFileURL
    : input.deletingLastPathComponent().appendingPathComponent("NBA球队实力蜂群图.png")
guard output.pathExtension.lowercased() == "png" else { fail("输出文件须使用 .png 扩展名") }

let data: ChartData
do {
    data = try JSONDecoder().decode(ChartData.self, from: Data(contentsOf: input))
} catch {
    fail("无法读取制图数据：\(error)")
}

let teams = data.teams.sorted { $0.strength < $1.strength }
let codes = teams.map { $0.team.uppercased() }
let invalidCodeCharacters = CharacterSet(charactersIn: "ABCDEFGHIJKLMNOPQRSTUVWXYZ").inverted
guard (1...30).contains(teams.count), Set(codes).count == teams.count,
      codes.allSatisfy({ $0.count == 3 && $0.rangeOfCharacter(from: invalidCodeCharacters) == nil }),
      teams.allSatisfy({ $0.strength.isFinite }) else {
    fail("请提供 1 至 30 支不重复的球队、三字母缩写及有效评分")
}

let logoDirectory = URL(fileURLWithPath: #filePath).standardizedFileURL
    .deletingLastPathComponent().appendingPathComponent("素材/NBA球队LOGO", isDirectory: true)
let canvas = CGRect(x: 0, y: 0, width: 864, height: 648)
let panel = CGRect(x: 28, y: 106, width: canvas.width - 56, height: 500)
let markerArea = panel.insetBy(dx: 0, dy: 32)
let scale = 200.0 / 72
let preferredDiameter = 0.68 * 72

let low = teams.first!.strength
let high = teams.last!.strength
let padding = (max(11.5, high) - min(-5, low)) * 0.04
let xMin = data.xRange?.min ?? min(-5, low - padding)
let xMax = data.xRange?.max ?? max(11.5, high + padding)
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
    for team in teams {
        let centerX = x(team.strength)
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
let subtitleColor = color(0x5a5855)
let title = NSMutableAttributedString(attributedString: run(data.titlePrefix ?? "", 28, ink, mono: true, bold: true))
title.append(run(data.title, 28, ink, bold: true))
drawHeading(title, at: 28)

let subtitle = NSMutableAttributedString(string: "")
let subtitleParts = data.subtitle ?? [
    .init(label: "", value: data.method, style: nil),
    .init(label: "制图: ", value: data.credit, style: .credit),
    .init(label: "数据源: ", value: data.source, style: .mono),
    .init(label: "截至 ", value: data.asOf, style: .mono),
]
let subtitleSeparator = data.subtitle == nil ? " | " : "｜"
for part in subtitleParts {
    guard let value = part.value, !value.isEmpty else { continue }
    let separator = subtitle.length == 0 ? "" : subtitleSeparator
    subtitle.append(run(separator + part.label, 12.5, subtitleColor))
    let isCredit = part.style == .credit
    subtitle.append(run(value, isCredit ? 18 : 12.5, isCredit ? .black : subtitleColor,
                        mono: part.style == .mono, bold: isCredit))
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
    let label = run(tick > 0 ? "+" + number : number, 13, tickColor, mono: true)
    label.draw(at: CGPoint(x: tickX - label.size().width / 2, y: panel.maxY + 4))
}

let logoSize = diameter * 0.8
for (code, center) in zip(codes, centers) {
    let file = logoDirectory.appendingPathComponent("\(code).webp")
    guard let logo = NSImage(contentsOf: file), logo.isValid else {
        fail("无法读取 \(code) 的球队 Logo：\(file.path)")
    }
    let rect = CGRect(x: center.x - logoSize / 2, y: center.y - logoSize / 2,
                      width: logoSize, height: logoSize)
    logo.draw(in: rect, from: .zero, operation: .sourceOver, fraction: 1, respectFlipped: true,
              hints: [.interpolation: NSImageInterpolation.high])
}

let weak = run("← \(data.leftLabel ?? "实力较弱")", 15.6, ink, bold: true)
let strong = run("\(data.rightLabel ?? "实力较强") →", 15.6, ink, bold: true)
let edgeInset = panel.width * 0.15 / 16.5
weak.draw(at: CGPoint(x: panel.minX + edgeInset, y: panel.maxY - 27))
strong.draw(at: CGPoint(x: panel.maxX - edgeInset - strong.size().width, y: panel.maxY - 27))

bitmap.size = canvas.size
guard let png = bitmap.representation(using: .png, properties: [:]) else { fail("无法编码 PNG") }
do {
    try FileManager.default.createDirectory(at: output.deletingLastPathComponent(), withIntermediateDirectories: true)
    try png.write(to: output, options: .atomic)
} catch {
    fail("无法保存图表：\(error.localizedDescription)")
}
print("已保存 \(teams.count) 支球队的图表：\(output.path)")
