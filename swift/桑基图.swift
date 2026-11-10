import AppKit

struct ChartInput: Decodable {
    struct Group: Decodable {
        struct Leaf: Decodable {
            let key: String
            let label: String
        }
        let label: String
        let leaves: [Leaf]
    }

    struct Row: Decodable {
        let name: String
        let values: [String: Double]
        let color: String?
        let avatar: String?
    }

    let title: String
    let totalLabel: String
    let subtitle: String?
    let credit: String?
    let language: String?
    let columns: Int?
    let groups: [Group]
    let rows: [Row]
}

struct Span {
    let top: CGFloat
    let bottom: CGFloat
    var center: CGFloat { (top + bottom) / 2 }
}

func fail(_ message: String) -> Never {
    FileHandle.standardError.write(Data((message + "\n").utf8))
    exit(1)
}

func color(_ hex: UInt32, alpha: CGFloat = 1) -> NSColor {
    NSColor(srgbRed: CGFloat((hex >> 16) & 255) / 255,
            green: CGFloat((hex >> 8) & 255) / 255,
            blue: CGFloat(hex & 255) / 255, alpha: alpha)
}

func rgb(_ value: String) -> UInt32? {
    guard value.count == 7, value.hasPrefix("#") else { return nil }
    return UInt32(value.dropFirst(), radix: 16)
}

func hasText(_ value: String) -> Bool {
    !value.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
}

let args = CommandLine.arguments
guard (2...3).contains(args.count) else {
    fail("用法：swift 桑基图.swift /绘图输入.json [/输出.png]")
}
let input = URL(fileURLWithPath: args[1]).standardizedFileURL
let directory = input.deletingLastPathComponent()
let root = URL(fileURLWithPath: #filePath).standardizedFileURL.deletingLastPathComponent().deletingLastPathComponent()
let output = args.count == 3 ? URL(fileURLWithPath: args[2]).standardizedFileURL
    : root.appendingPathComponent("成品图/桑基图.png")
guard output.pathExtension.lowercased() == "png",
      output.resolvingSymlinksInPath() != input.resolvingSymlinksInPath() else {
    fail("输出须为独立的 PNG 路径")
}

let data: ChartInput
do {
    data = try JSONDecoder().decode(ChartInput.self, from: Data(contentsOf: input))
} catch {
    fail("无法读取绘图输入：\(error)")
}
let language = data.language ?? "zh-Hans"
let requestedColumns = data.columns ?? 4
guard hasText(data.title), hasText(data.totalLabel), ["zh-Hans", "zh-Hant", "en"].contains(language),
      (1...5).contains(requestedColumns), data.groups.count == 2,
      data.groups.allSatisfy({ hasText($0.label) && !$0.leaves.isEmpty }), !data.rows.isEmpty else {
    fail("请提供标题、总量标签、两个分组和至少一个对象，并检查语言及列数")
}
let leaves = data.groups.flatMap(\.leaves)
let keys = Set(leaves.map(\.key))
guard leaves.count <= 6, keys.count == leaves.count,
      leaves.allSatisfy({ hasText($0.key) && hasText($0.label) }) else {
    fail("两个分组最多共用六个子类，每个子类须有唯一的 key 和非空 label")
}

func names(_ row: ChartInput.Row) -> [String] {
    row.name.trimmingCharacters(in: .whitespacesAndNewlines)
        .split(omittingEmptySubsequences: false, whereSeparator: \.isNewline).map(String.init)
}

func avatarURL(_ path: String) -> URL {
    path.hasPrefix("/") ? URL(fileURLWithPath: path) : directory.appendingPathComponent(path)
}

for (index, row) in data.rows.enumerated() {
    let lines = names(row)
    guard (1...2).contains(lines.count), lines.allSatisfy(hasText), Set(row.values.keys) == keys,
          row.values.values.allSatisfy({ $0.isFinite && $0 >= 0 }) else {
        fail("第 \(index + 1) 行名称或子类数值无效")
    }
    if let value = row.color, rgb(value) == nil { fail("第 \(index + 1) 行颜色须为 #RRGGBB") }
}
let maximum = data.rows.map { $0.values.values.reduce(0, +) }.max() ?? 0
guard maximum.isFinite, maximum > 0 else { fail("至少一个子类数值必须大于 0，且总量须为有限数") }

let panelW: CGFloat = 8.5, panelH: CGFloat = 6.8
let chartTop = panelH - 1.42
let x1: CGFloat = 1.45, x2: CGFloat = 3.60, x3: CGFloat = 5.95, nodeW: CGFloat = 0.20
let headerHeight = 272, panelPixelsH = 735
let panelScale = CGFloat(panelPixelsH) / panelH
let panelPixelsW = Int((panelW * panelScale).rounded())
let columns = min(requestedColumns, data.rows.count)
let rowCount = (data.rows.count + columns - 1) / columns
let width = Int((CGFloat(columns) * panelW * 110).rounded())
let height = headerHeight + rowCount * panelPixelsH
let left = (width - columns * panelPixelsW) / 2
let split = data.groups[0].leaves.count
var gaps = [CGFloat](repeating: 0.31, count: leaves.count)
gaps[0] = 0
gaps[split] = 0.35
let perUnit = (chartTop - gaps.reduce(0, +) - 0.24) / CGFloat(maximum)
let groupColors: [UInt32] = [0x8C3A34, 0x3C6575]
let palettes: [[UInt32]] = [[0x9E413A, 0xB7746B, 0xAC6155, 0x78423F, 0xD09287],
                            [0x56899A, 0x78A3B1, 0x315967, 0x467685, 0x97B5BA]]
let leafColors = Array(palettes[0].prefix(split)) + Array(palettes[1].prefix(leaves.count - split))
let ink = color(0x2B2B2B), gray = color(0x7A7A7A)
let styles = try JSONSerialization.jsonObject(with: Data(contentsOf: root.appendingPathComponent("样式.json")))
    as? [String: [String: Any]] ?? [:]
let heading = (styles["header"] ?? [:]).merging(styles["桑基图"] ?? [:]) { _, value in value }
guard let titleSize = heading["title"] as? Double, let subtitleSize = heading["subtitle"] as? Double,
      let titleBold = heading["titleBold"] as? Bool, let subtitleBold = heading["subtitleBold"] as? Bool,
      let titleHex = rgb(heading["titleColor"] as? String ?? ""),
      let subtitleHex = rgb(heading["subtitleColor"] as? String ?? "") else {
    fail("样式.json 缺少有效的标题样式")
}

guard let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: width, pixelsHigh: height,
                                    bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
                                    isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0),
      let graphics = NSGraphicsContext(bitmapImageRep: bitmap) else { fail("无法创建绘图画布") }
let context = graphics.cgContext
context.translateBy(x: 0, y: CGFloat(height))
context.scaleBy(x: 1, y: -1)
context.interpolationQuality = .high
NSGraphicsContext.current = NSGraphicsContext(cgContext: context, flipped: true)
context.setFillColor(color(0xF3F4F5).cgColor)
context.fill(CGRect(x: 0, y: 0, width: CGFloat(width), height: CGFloat(height)))

enum Align { case start, center, end }

func face(_ size: CGFloat, bold: Bool = false, mono: Bool = false) -> NSFont {
    let weight: NSFont.Weight = bold ? .semibold : .regular
    if mono {
        return NSFont(name: bold ? "Menlo-Bold" : "Menlo-Regular", size: size)
            ?? NSFont.monospacedSystemFont(ofSize: size, weight: weight)
    }
    let family = language == "zh-Hant" ? "PingFangTC" : "PingFangSC"
    if language != "en", let font = NSFont(name: family + (bold ? "-Semibold" : "-Regular"), size: size) {
        return font
    }
    return NSFont.systemFont(ofSize: size, weight: weight)
}

func run(_ value: String, _ font: NSFont, _ foreground: NSColor) -> NSAttributedString {
    NSAttributedString(string: value, attributes: [.font: font, .foregroundColor: foreground])
}

func offset(_ size: CGFloat, _ alignment: Align) -> CGFloat {
    switch alignment {
    case .start: return 0
    case .center: return size / 2
    case .end: return size
    }
}

func drawText(_ value: String, at point: CGPoint, font: NSFont, foreground: NSColor = ink,
              h: Align = .center, v: Align = .center, limit: CGFloat? = nil) {
    var text = run(value, font, foreground)
    if let limit = limit, text.size().width > limit {
        let size = max(1, font.pointSize * limit / text.size().width)
        text = run(value, NSFont(descriptor: font.fontDescriptor, size: size) ?? font, foreground)
    }
    let size = text.size()
    text.draw(at: CGPoint(x: point.x - offset(size.width, h), y: point.y - offset(size.height, v)))
}

func drawHeader() {
    let subtitle = data.subtitle ?? "", credit = data.credit ?? ""
    let hasSubtitle = !subtitle.isEmpty || !credit.isEmpty
    let limit = CGFloat(width) - 80
    drawText(data.title, at: CGPoint(x: CGFloat(width) / 2, y: hasSubtitle ? 24 : 60),
             font: face(titleSize, bold: titleBold), foreground: color(titleHex), v: .start, limit: limit)
    guard hasSubtitle else { return }

    func subtitleRun(_ size: CGFloat) -> NSAttributedString {
        let text = NSMutableAttributedString(attributedString:
            run(subtitle + (!subtitle.isEmpty && !credit.isEmpty ? " | " : ""),
                face(size, bold: subtitleBold), color(subtitleHex)))
        text.append(run(credit, face(size, bold: true), .black))
        return text
    }
    var text = subtitleRun(subtitleSize)
    if text.size().width > limit { text = subtitleRun(subtitleSize * limit / text.size().width) }
    text.draw(at: CGPoint(x: (CGFloat(width) - text.size().width) / 2, y: 164))
}

func drawPanel(_ row: ChartInput.Row, at origin: CGPoint) {
    func point(_ x: CGFloat, _ y: CGFloat) -> CGPoint {
        CGPoint(x: origin.x + x * panelScale, y: origin.y + (panelH - y) * panelScale)
    }

    func ribbon(_ xa: CGFloat, _ xb: CGFloat, _ start: Span, _ end: Span, _ hex: UInt32) {
        guard start.top > start.bottom else { return }
        let step = (xb - xa) / 3
        let path = CGMutablePath()
        path.move(to: point(xa, start.top))
        path.addCurve(to: point(xb, end.top), control1: point(xa + step, start.top),
                      control2: point(xb - step, end.top))
        path.addLine(to: point(xb, end.bottom))
        path.addCurve(to: point(xa, start.bottom), control1: point(xb - step, end.bottom),
                      control2: point(xa + step, start.bottom))
        path.closeSubpath()
        context.setFillColor(color(hex, alpha: 0.55).cgColor)
        context.addPath(path)
        context.fillPath()
    }

    func node(_ x: CGFloat, _ span: Span, _ hex: UInt32) {
        guard span.top > span.bottom else { return }
        let topLeft = point(x, span.top)
        context.setFillColor(color(hex).cgColor)
        context.fill(CGRect(x: topLeft.x, y: topLeft.y, width: nodeW * panelScale,
                            height: (span.top - span.bottom) * panelScale))
    }

    let values = leaves.map { row.values[$0.key, default: 0] }
    let midValues = [values.prefix(split).reduce(0, +), values.dropFirst(split).reduce(0, +)]
    let total = midValues.reduce(0, +)
    var cursor = chartTop
    var leafSpans: [Span] = []
    for (value, gap) in zip(values, gaps) {
        let top = cursor - gap
        cursor = top - CGFloat(value) * perUnit
        leafSpans.append(Span(top: top, bottom: cursor))
    }
    let yc = (chartTop + cursor) / 2
    let halfTotal = CGFloat(total) * perUnit / 2
    let totalSpan = Span(top: yc + halfTotal, bottom: yc - halfTotal)
    let splitY = totalSpan.top - CGFloat(midValues[0]) * perUnit
    let sourceSpans = [Span(top: totalSpan.top, bottom: splitY), Span(top: splitY, bottom: totalSpan.bottom)]
    let midCenters = [(leafSpans[0].top + leafSpans[split - 1].bottom) / 2,
                      (leafSpans[split].top + leafSpans[leafSpans.count - 1].bottom) / 2]
    let midSpans = zip(midCenters, midValues).map { center, value in
        let half = CGFloat(value) * perUnit / 2
        return Span(top: center + half, bottom: center - half)
    }
    for i in 0..<2 {
        ribbon(x1 + nodeW, x2, sourceSpans[i], midSpans[i], groupColors[i])
        cursor = midSpans[i].top
        let indices = i == 0 ? 0..<split : split..<leaves.count
        for j in indices {
            let next = cursor - CGFloat(values[j]) * perUnit
            ribbon(x2 + nodeW, x3, Span(top: cursor, bottom: next), leafSpans[j], leafColors[j])
            cursor = next
        }
    }
    node(x1, totalSpan, row.color.flatMap(rgb) ?? 0x555555)
    for (span, hex) in zip(midSpans, groupColors) { node(x2, span, hex) }
    for (span, hex) in zip(leafSpans, leafColors) { node(x3, span, hex) }

    func number(_ value: Double) -> String { String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), value) }
    let labels = face(language == "en" ? 28 : 32)
    drawText(number(total), at: point(x1 - 0.16, yc + 0.06), font: face(62, bold: true, mono: true),
             h: .end, v: .end, limit: 1.12 * panelScale)
    drawText(data.totalLabel, at: point(x1 - 0.16, yc - 0.06), font: labels,
             foreground: gray, h: .end, v: .start, limit: 1.12 * panelScale)
    let xc = (x1 + nodeW + x2) / 2
    var positions = zip(sourceSpans, midCenters).map { ($0.0.center + $0.1) / 2 }
    if midValues.allSatisfy({ $0 > 0 }), positions[0] - positions[1] < 0.75 {
        let center = (positions[0] + positions[1]) / 2
        positions = [center + 0.375, center - 0.375]
    }
    for i in 0..<2 where midValues[i] > 0 {
        drawText(number(midValues[i]), at: point(xc, positions[i] + 0.06), font: face(47, bold: true, mono: true),
                 v: .end, limit: 1.75 * panelScale)
        drawText(data.groups[i].label, at: point(xc, positions[i] - 0.06), font: labels,
                 v: .start, limit: 1.75 * panelScale)
    }
    let xl = x3 + nodeW + 0.15
    let gap: CGFloat = language == "en" ? 1.76 : 1.34
    for i in leaves.indices {
        let y = leafSpans[i].center
        drawText(leaves[i].label, at: point(xl, y), font: labels, h: .start, limit: (gap - 0.10) * panelScale)
        drawText(number(values[i]), at: point(xl + gap, y), font: face(34, bold: true, mono: true),
                 h: .start, limit: (panelW - xl - gap - 0.04) * panelScale)
    }

    let lines = names(row)
    let containsCJK = row.name.unicodeScalars.contains { $0.value >= 0x3000 }
    var nameFont = face(40, mono: !containsCJK)
    var nameWidth = lines.map { run($0, nameFont, ink).size().width }.max() ?? 0
    let limit = 3.4 * panelScale
    if nameWidth > limit {
        nameFont = face(40 * limit / nameWidth, mono: !containsCJK)
        nameWidth = lines.map { run($0, nameFont, ink).size().width }.max() ?? 0
    }
    let center = point((x1 + x3 + nodeW) / 2, chartTop + 0.72)
    let side: CGFloat = row.avatar == nil ? 0 : panelScale.rounded()
    let nameGap: CGFloat = row.avatar == nil ? 0 : 0.16 * panelScale
    let headLeft = center.x - (side + nameGap + nameWidth) / 2
    if let path = row.avatar {
        guard let avatar = NSImage(contentsOf: avatarURL(path)), avatar.isValid else { fail("无法读取头像：\(path)") }
        let rect = CGRect(x: headLeft, y: center.y - side / 2, width: side, height: side)
        avatar.draw(in: rect, from: .zero, operation: .sourceOver, fraction: 1,
                    respectFlipped: true, hints: [.interpolation: NSImageInterpolation.high])
    }
    for (index, line) in lines.enumerated() {
        let y = center.y + (CGFloat(index) - CGFloat(lines.count - 1) / 2) * 40
        drawText(line, at: CGPoint(x: headLeft + side + nameGap, y: y), font: nameFont, h: .start)
    }
}

drawHeader()
for (index, row) in data.rows.enumerated() {
    let origin = CGPoint(x: CGFloat(left + (index % columns) * panelPixelsW),
                         y: CGFloat(headerHeight + (index / columns) * panelPixelsH))
    drawPanel(row, at: origin)
}
bitmap.size = NSSize(width: CGFloat(width) * 72 / 110, height: CGFloat(height) * 72 / 110)
guard let png = bitmap.representation(using: .png, properties: [:]) else { fail("无法编码 PNG") }
do {
    try FileManager.default.createDirectory(at: output.deletingLastPathComponent(), withIntermediateDirectories: true)
    try png.write(to: output, options: .atomic)
} catch {
    fail("无法保存图表：\(error)")
}
print("\(width) × \(height)  \(output.path)")
