import AppKit
import Foundation

// 数据点按输入顺序绘制，后绘制的队标覆盖先绘制的队标。
struct Axis: Decodable {
    let label: String
    let low: String?, high: String?
    let reverse: Bool?
    let limits: [Double]?
}

struct Point: Decodable {
    let x: Double, y: Double
    let logo: String?
}

struct Chart: Decodable {
    enum Reference: String, Decodable { case median, mean }

    let title: String
    let subtitle: String?
    let xAxis: Axis, yAxis: Axis
    let points: [Point]
    let quadrants: [String: String]?
    let quadrantColors: [String: String]?
    let reference: Reference?
    let logoSize: Double?
    let glow: Bool?
}

struct ChartError: LocalizedError {
    let message: String
    var errorDescription: String? { message }
}

func domain(_ axis: Axis, _ values: [Double], paddingRatio: Double) throws -> ClosedRange<Double> {
    let minimum = values.min()!, maximum = values.max()!
    if let limits = axis.limits {
        guard limits.count == 2, limits[0] < limits[1], limits[0] <= minimum, limits[1] >= maximum else {
            throw ChartError(message: "坐标范围须由两个递增的数值组成，并包含所有数据点")
        }
        return limits[0]...limits[1]
    }
    let padding = maximum > minimum ? (maximum - minimum) * paddingRatio : max(1, abs(minimum) * 0.05)
    return (minimum - padding)...(maximum + padding)
}

func fraction(_ value: Double, _ range: ClosedRange<Double>, _ reverse: Bool?) -> Double {
    let part = (value - range.lowerBound) / (range.upperBound - range.lowerBound)
    return reverse == true ? 1 - part : part
}

func ticks(_ range: ClosedRange<Double>) -> [Double] {
    let target = (range.upperBound - range.lowerBound) / 6
    let magnitude = pow(10, floor(log10(target)))
    let step = [1.0, 2, 2.5, 5, 10].map { $0 * magnitude }.first { $0 >= target } ?? magnitude * 10
    let first = ceil(range.lowerBound / step) * step
    return (0...6).map { first + Double($0) * step }.filter { $0 <= range.upperBound + step * 1e-9 }
}

func median(_ values: [Double]) -> Double {
    let sorted = values.sorted(), mid = sorted.count / 2
    return sorted.count.isMultiple(of: 2) ? (sorted[mid - 1] + sorted[mid]) / 2 : sorted[mid]
}

func render(_ chart: Chart, input: URL, output: URL, root: URL) throws {
    guard !chart.points.isEmpty else { throw ChartError(message: "至少需要一个数据点") }
    let logoSize = chart.logoSize ?? 80
    guard (1...200).contains(logoSize) else { throw ChartError(message: "logoSize 须在 1 到 200 之间") }
    let styles = try JSONSerialization.jsonObject(with: Data(contentsOf: root.appendingPathComponent("样式.json")))
        as? [String: [String: Any]] ?? [:]
    let heading = (styles["header"] ?? [:]).merging(styles["散点图"] ?? [:]) { _, value in value }
    guard let padding = heading["axisPadding"] as? Double, (0...1).contains(padding) else {
        throw ChartError(message: "样式.json 的 axisPadding 须在 0 到 1 之间")
    }
    let xs = chart.points.map(\.x), ys = chart.points.map(\.y)
    let xDomain = try domain(chart.xAxis, xs, paddingRatio: padding)
    let yDomain = try domain(chart.yAxis, ys, paddingRatio: padding)
    let logos = try chart.points.map { point -> NSImage? in
        guard let path = point.logo else { return nil }
        let url = URL(fileURLWithPath: path, relativeTo: input.deletingLastPathComponent())
        guard let image = NSImage(contentsOf: url), image.size.width > 0, image.size.height > 0 else {
            throw ChartError(message: "无法读取素材：\(path)")
        }
        return image
    }
    let width = 1600.0, height = 1200.0, scale = 2.0
    let plot = CGRect(x: 116, y: 143, width: 1436, height: 958)
    let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: Int(width * scale),
                                 pixelsHigh: Int(height * scale), bitsPerSample: 8, samplesPerPixel: 4,
                                 hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB,
                                 bytesPerRow: 0, bitsPerPixel: 0)!
    let context = NSGraphicsContext(bitmapImageRep: bitmap)!.cgContext
    context.translateBy(x: 0, y: height * scale)
    context.scaleBy(x: scale, y: -scale)
    NSGraphicsContext.current = NSGraphicsContext(cgContext: context, flipped: true)
    func color(_ r: Double, _ g: Double, _ b: Double, _ alpha: Double = 1) -> NSColor {
        NSColor(srgbRed: r / 255, green: g / 255, blue: b / 255, alpha: alpha)
    }
    let paper = color(255, 250, 240), ink = color(44, 62, 80), soft = color(86, 100, 115)
    func hexColor(_ value: String?) -> NSColor? {
        guard let value, value.count == 7, value.hasPrefix("#"),
              let rgb = UInt32(value.dropFirst(), radix: 16) else { return nil }
        return color(Double((rgb >> 16) & 255), Double((rgb >> 8) & 255), Double(rgb & 255))
    }
    guard let titleSize = heading["title"] as? Double, let subtitleSize = heading["subtitle"] as? Double,
          let titleBold = heading["titleBold"] as? Bool, let subtitleBold = heading["subtitleBold"] as? Bool,
          let titleColor = hexColor(heading["titleColor"] as? String),
          let subtitleColor = hexColor(heading["subtitleColor"] as? String) else {
        throw ChartError(message: "样式.json 缺少有效的标题样式")
    }
    let quadrantColors = try (chart.quadrantColors ?? [:]).mapValues { value in
        guard let tint = hexColor(value) else { throw ChartError(message: "象限颜色须为 #RRGGBB") }
        return tint
    }
    let grid = color(230, 226, 217), guide = color(114, 120, 119)
    context.setFillColor(paper.cgColor)
    context.fill(CGRect(x: 0, y: 0, width: width, height: height))

    func font(_ size: Double, _ bold: Bool) -> NSFont {
        let primary = NSFont(name: bold ? "Menlo-Bold" : "Menlo-Regular", size: size)!
        let cjk = NSFontDescriptor(name: bold ? "PingFangSC-Semibold" : "PingFangSC-Regular", size: size)
        return NSFont(descriptor: primary.fontDescriptor.addingAttributes([.cascadeList: [cjk]]), size: size)!
    }
    func text(_ label: String, _ x: Double, _ y: Double, _ size: Double,
              _ fill: NSColor, bold: Bool = false, align: Double = 0, maxWidth: Double? = nil) {
        guard !label.isEmpty else { return }
        let measure = (label as NSString).size(withAttributes: [.font: font(size, bold)]).width
        let adjusted = min(size, max(1, size * (maxWidth ?? measure) / max(1, measure)))
        let string = NSAttributedString(string: label, attributes: [.font: font(adjusted, bold), .foregroundColor: fill])
        string.draw(at: CGPoint(x: x - align * string.size().width, y: y))
    }
    func line(_ start: CGPoint, _ end: CGPoint, _ fill: NSColor, dashed: Bool = false) {
        context.saveGState()
        context.setStrokeColor(fill.cgColor)
        context.setLineWidth(dashed ? 1.45 : 0.85)
        if dashed { context.setLineDash(phase: 0, lengths: [6, 7]) }
        context.move(to: start)
        context.addLine(to: end)
        context.strokePath()
        context.restoreGState()
    }
    func x(_ value: Double) -> Double { plot.minX + plot.width * fraction(value, xDomain, chart.xAxis.reverse) }
    func y(_ value: Double) -> Double { plot.maxY - plot.height * fraction(value, yDomain, chart.yAxis.reverse) }
    func position(_ point: Point) -> CGPoint { CGPoint(x: x(point.x), y: y(point.y)) }
    func referenceValue(_ values: [Double]) -> Double {
        chart.reference == .mean ? values.reduce(0, +) / Double(values.count) : median(values)
    }
    let midX = x(referenceValue(xs)), midY = y(referenceValue(ys))
    let green = color(114, 196, 165), red = color(233, 157, 171)
    if chart.glow == true {
        context.saveGState()
        context.clip(to: plot)
        for point in chart.points {
            let p = position(point)
            let dx = (p.x - midX) / max(1, p.x >= midX ? plot.maxX - midX : midX - plot.minX)
            let dy = (midY - p.y) / max(1, p.y <= midY ? midY - plot.minY : plot.maxY - midY)
            let score = (dx + dy) / 2
            let weight = min(1, max(0, (abs(score) - 0.14) / 0.64))
            if weight == 0 { continue }
            let tint = score > 0 ? green : red
            let colors = [0.62, 0.36, 0.10, 0].map { tint.withAlphaComponent($0 * weight).cgColor }
            let gradient = CGGradient(colorsSpace: CGColorSpace(name: CGColorSpace.sRGB),
                                      colors: colors as CFArray, locations: [0, 0.35, 0.68, 1])!
            context.drawRadialGradient(gradient, startCenter: p, startRadius: 0, endCenter: p,
                                       endRadius: 42 + 133 * weight, options: [])
        }
        context.restoreGState()
    }
    for tick in ticks(xDomain) {
        let value = x(tick)
        line(CGPoint(x: value, y: plot.minY), CGPoint(x: value, y: plot.maxY), grid)
        text(String(format: "%g", tick == 0 ? 0 : tick), value, plot.maxY + 6, 17, soft, align: 0.5)
    }
    for tick in ticks(yDomain) {
        let value = y(tick)
        line(CGPoint(x: plot.minX, y: value), CGPoint(x: plot.maxX, y: value), grid)
        text(String(format: "%g", tick == 0 ? 0 : tick), plot.minX - 10, value - 10, 17, soft, align: 1)
    }
    line(CGPoint(x: midX, y: plot.minY), CGPoint(x: midX, y: plot.maxY), guide, dashed: true)
    line(CGPoint(x: plot.minX, y: midY), CGPoint(x: plot.maxX, y: midY), guide, dashed: true)
    for (point, logo) in zip(chart.points, logos) {
        let p = position(point)
        if let logo {
            let factor = logoSize / max(logo.size.width, logo.size.height)
            let w = logo.size.width * factor, h = logo.size.height * factor
            logo.draw(in: CGRect(x: p.x - w / 2, y: p.y - h / 2, width: w, height: h),
                      from: .zero, operation: .sourceOver, fraction: 1, respectFlipped: true,
                      hints: [.interpolation: NSImageInterpolation.high])
        } else {
            context.setFillColor(ink.cgColor)
            context.fillEllipse(in: CGRect(x: p.x - 5, y: p.y - 5, width: 10, height: 10))
        }
    }
    let corners: [(String, Double, Double, Double, NSColor)] = [
        ("topLeft", plot.minX + 23, plot.minY + 25, 0, soft),
        ("topRight", plot.maxX - 23, plot.minY + 25, 1, color(37, 92, 81)),
        ("bottomLeft", plot.minX + 23, plot.maxY - 54, 0, color(133, 70, 83)),
        ("bottomRight", plot.maxX - 23, plot.maxY - 54, 1, soft)
    ]
    for (key, x, y, alignment, fill) in corners {
        if let label = chart.quadrants?[key] {
            text(label, x, y, 28, quadrantColors[key] ?? fill, bold: true, align: alignment)
        }
    }
    func axisTitle(_ axis: Axis) -> String {
        var parts = [axis.label]
        if let low = axis.low, !low.isEmpty { parts.insert("<——\(low)", at: 0) }
        if let high = axis.high, !high.isEmpty { parts.append("\(high)——>") }
        return parts.joined(separator: "      ")
    }
    text(chart.title, 40, 25, titleSize, titleColor, bold: titleBold, maxWidth: width - 80)
    if let subtitle = chart.subtitle {
        text(subtitle, 40, 91, subtitleSize, subtitleColor, bold: subtitleBold, maxWidth: width - 80)
    }
    text(axisTitle(chart.xAxis), plot.midX, plot.maxY + 44, 20, ink, bold: true, align: 0.5, maxWidth: plot.width)
    context.saveGState()
    context.translateBy(x: 35, y: plot.midY)
    context.rotate(by: -.pi / 2)
    text(axisTitle(chart.yAxis), 0, 0, 20, ink, bold: true, align: 0.5, maxWidth: plot.height)
    context.restoreGState()

    try FileManager.default.createDirectory(at: output.deletingLastPathComponent(), withIntermediateDirectories: true)
    guard let png = bitmap.representation(using: .png, properties: [:]) else {
        throw ChartError(message: "无法编码 PNG")
    }
    try png.write(to: output, options: .atomic)
    print("已保存 \(output.path)（3200×2400）")
}

guard (2...3).contains(CommandLine.arguments.count) else {
    print("用法：swift 散点图.swift 数据.json [成品.png]")
    exit(CommandLine.arguments.count == 1 ? 0 : 1)
}
do {
    let root = URL(fileURLWithPath: #filePath).standardizedFileURL.deletingLastPathComponent().deletingLastPathComponent()
    let input = URL(fileURLWithPath: CommandLine.arguments[1]).standardizedFileURL
    let output = CommandLine.arguments.count == 3
        ? URL(fileURLWithPath: CommandLine.arguments[2]).standardizedFileURL
        : root.appendingPathComponent("成品图/散点图.png")
    guard output.pathExtension.lowercased() == "png",
          output.resolvingSymlinksInPath() != input.resolvingSymlinksInPath() else {
        throw ChartError(message: "请指定独立的 PNG 输出路径")
    }
    let chart = try JSONDecoder().decode(Chart.self, from: Data(contentsOf: input))
    try render(chart, input: input, output: output, root: root)
} catch {
    FileHandle.standardError.write(Data("绘图失败：\(error.localizedDescription)\n".utf8))
    exit(1)
}
