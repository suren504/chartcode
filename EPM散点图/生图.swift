import AppKit
import CoreFoundation
import Foundation

// 队标始终以原始数据坐标为中心，重叠时按输入顺序叠放。
struct Axis: Decodable {
    let label: String
    let low: String?, high: String?
    let reverse: Bool?
    let limits: [Double]?
    let percent: Bool?
}

struct Point: Decodable {
    struct Label: Decodable {
        enum Side: String, Decodable { case above, below }

        let text: String, color: String
        let side: Side
        let offsetX: Double?
        let gap: Double?
    }

    let x: Double, y: Double
    let logo: String?
    let label: Label?
}

struct Chart: Decodable {
    enum Reference: String, Decodable { case median, mean, zero }

    struct Connection: Decodable {
        let pointIndices: [Int]
        let color: String
    }

    let title: String
    let subtitle: String?
    let subtitleEmphasis: String?
    let xAxis: Axis, yAxis: Axis
    let points: [Point]
    let connections: [Connection]?
    let quadrants: [String: String]?
    let quadrantColors: [String: String]?
    let quadrantOffsetsX: [String: Double]?
    let backgroundColor: String?
    let backgroundImage: String?
    let reference: Reference?
    let logoSize: Double?
    let dotSize: Double?
    let glow: Bool?
    let glowPalette: [String]?
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

func tickLabel(_ value: Double, axis: Axis) -> String {
    let tick = value == 0 ? 0 : value
    if axis.percent == true {
        return String(format: "%g%%", tick * 100)
    }
    return String(format: "%g", tick)
}

func median(_ values: [Double]) -> Double {
    let sorted = values.sorted(), mid = sorted.count / 2
    return sorted.count.isMultiple(of: 2) ? (sorted[mid - 1] + sorted[mid]) / 2 : sorted[mid]
}

func axisTitle(_ axis: Axis, name: String) throws -> String {
    let label = axis.label.trimmingCharacters(in: .whitespacesAndNewlines)
    let low = axis.low?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
    let high = axis.high?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
    if !label.isEmpty, axis.low == nil, axis.high == nil { return label }
    guard !label.isEmpty, !low.isEmpty, !high.isEmpty else {
        throw ChartError(message: "\(name) 须填写轴名称 label 和两端文案 low、high")
    }
    return "<——\(low)  \(label)  \(high)——>"
}

func render(_ chart: Chart, input: URL, output: URL) throws {
    guard !chart.points.isEmpty else { throw ChartError(message: "至少需要一个数据点") }
    let xTitle = try axisTitle(chart.xAxis, name: "X 轴")
    let yTitle = try axisTitle(chart.yAxis, name: "Y 轴")
    guard let quadrants = chart.quadrants, ["topRight", "bottomLeft"].allSatisfy({ key in
        guard let label = quadrants[key] else { return false }
        return !label.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
    }) else {
        throw ChartError(message: "quadrants 须填写右上角 topRight 和左下角 bottomLeft 的文案")
    }
    let logoSize = chart.logoSize ?? 80
    guard (1...200).contains(logoSize) else { throw ChartError(message: "logoSize 须在 1 到 200 之间") }
    let dotSize = chart.dotSize ?? 10
    guard (1...200).contains(dotSize) else { throw ChartError(message: "dotSize 须在 1 到 200 之间") }
    let heading = epmStyle
    guard let padding = heading["axisPadding"] as? Double, (0...1).contains(padding) else {
        throw ChartError(message: "样式.json 的 axisPadding 须在 0 到 1 之间")
    }
    let xs = chart.points.map(\.x), ys = chart.points.map(\.y)
    let xDomain = try domain(chart.xAxis, xs, paddingRatio: padding)
    let yDomain = try domain(chart.yAxis, ys, paddingRatio: padding)
    if chart.reference == .zero, !xDomain.contains(0) || !yDomain.contains(0) {
        throw ChartError(message: "使用零值参考线时，两轴范围须包含 0")
    }
    func loadImage(_ path: String) throws -> NSImage {
        let url = URL(fileURLWithPath: path, relativeTo: input.deletingLastPathComponent())
        guard let image = NSImage(contentsOf: url), image.size.width > 0, image.size.height > 0 else {
            throw ChartError(message: "无法读取素材：\(path)")
        }
        return image
    }
    let logos = try chart.points.map { try $0.logo.map(loadImage) }
    let background = try chart.backgroundImage.map(loadImage)
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
    let ink = color(44, 62, 80), soft = color(86, 100, 115)
    func hexColor(_ value: String?) -> NSColor? {
        guard let value, value.count == 7, value.hasPrefix("#"),
              let rgb = UInt32(value.dropFirst(), radix: 16) else { return nil }
        return color(Double((rgb >> 16) & 255), Double((rgb >> 8) & 255), Double(rgb & 255))
    }
    guard let paper = hexColor(chart.backgroundColor ?? "#FFFAF0") else {
        throw ChartError(message: "背景颜色须为 #RRGGBB")
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
    let grid = background == nil ? color(230, 226, 217) : color(44, 62, 80, 0.14)
    let guide = color(114, 120, 119)
    context.setFillColor(paper.cgColor)
    context.fill(CGRect(x: 0, y: 0, width: width, height: height))
    if let background {
        let tile = background.size
        for tileY in stride(from: 0.0, to: height, by: tile.height) {
            for tileX in stride(from: 0.0, to: width, by: tile.width) {
                background.draw(in: CGRect(x: tileX, y: tileY, width: tile.width, height: tile.height),
                                from: .zero, operation: .sourceOver, fraction: 1, respectFlipped: true,
                                hints: [.interpolation: NSImageInterpolation.high])
            }
        }
    }

    func font(_ size: Double, _ bold: Bool) -> NSFont {
        let primary = NSFont(name: bold ? "Menlo-Bold" : "Menlo-Regular", size: size)!
        let names = bold ? ["PingFangSC-Semibold", "MicrosoftYaHei-Bold"] : ["PingFangSC-Regular", "MicrosoftYaHei"]
        let cjk = names.compactMap { NSFont(name: $0, size: size)?.fontDescriptor }
        return NSFont(descriptor: primary.fontDescriptor.addingAttributes([.cascadeList: cjk]), size: size)!
    }
    func text(_ label: String, _ x: Double, _ y: Double, _ size: Double,
              _ fill: NSColor, bold: Bool = false, align: Double = 0, maxWidth: Double? = nil,
              verticalAlign: Double = 0, emphasis: String? = nil) {
        guard !label.isEmpty else { return }
        let emphasisRange = emphasis.map { (label as NSString).range(of: $0) }
        func styledText(_ pointSize: Double) -> NSAttributedString {
            let attributes: [NSAttributedString.Key: Any] = [.font: font(pointSize, bold), .foregroundColor: fill]
            guard let range = emphasisRange, range.location != NSNotFound, range.length > 0 else {
                return NSAttributedString(string: label, attributes: attributes)
            }
            let string = NSMutableAttributedString(string: label, attributes: attributes)
            string.addAttributes([.font: font(pointSize * 1.3, true), .strokeWidth: -2, .strokeColor: fill], range: range)
            return string
        }
        var string = styledText(size)
        var bounds = string.size()
        if let maxWidth, bounds.width > maxWidth {
            string = styledText(max(1, size * maxWidth / bounds.width))
            bounds = string.size()
        }
        string.draw(at: CGPoint(x: x - align * bounds.width, y: y - verticalAlign * bounds.height))
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
        switch chart.reference {
        case .zero: return 0
        case .mean: return values.reduce(0, +) / Double(values.count)
        case .median, nil: return median(values)
        }
    }
    let midX = x(referenceValue(xs)), midY = y(referenceValue(ys))
    let palette = try chart.glowPalette.map { values in
        guard values.count >= 2 else { throw ChartError(message: "光晕色带至少需要两种颜色") }
        return try values.map { value in
            guard let color = hexColor(value) else { throw ChartError(message: "光晕颜色须为 #RRGGBB") }
            return color
        }
    }
    if chart.glow != false {
        let green = color(114, 196, 165), red = color(233, 157, 171)
        context.saveGState()
        context.clip(to: plot)
        for point in chart.points {
            let p = position(point)
            let dx = (p.x - midX) / max(1, p.x >= midX ? plot.maxX - midX : midX - plot.minX)
            let dy = (midY - p.y) / max(1, p.y <= midY ? midY - plot.minY : plot.maxY - midY)
            let score = (dx + dy) / 2
            let tint: NSColor, radius: CGFloat, opacity: CGFloat
            if let palette {
                let position = (score + 1) / 2 * Double(palette.count - 1)
                let index = min(Int(position), palette.count - 2)
                let a = palette[index], b = palette[index + 1], t = position - Double(index)
                func mix(_ u: CGFloat, _ v: CGFloat) -> CGFloat { ((u + (v - u) * t) * 255).rounded() }
                tint = color(mix(a.redComponent, b.redComponent), mix(a.greenComponent, b.greenComponent),
                             mix(a.blueComponent, b.blueComponent))
                radius = point.logo == nil ? dotSize / 2 + 21 : logoSize * 1.65
                opacity = 0.58
            } else {
                let weight = min(1, max(0, (abs(score) - 0.14) / 0.64))
                if weight == 0 { continue }
                tint = score > 0 ? green : red
                radius = 42 + 133 * weight
                opacity = 0.62 * weight
            }
            let colors = (0...32).map { step in
                tint.withAlphaComponent(opacity * pow(1 - Double(step) / 32, 1.7)).cgColor
            }
            let gradient = CGGradient(colorsSpace: CGColorSpace(name: CGColorSpace.sRGB),
                                      colors: colors as CFArray, locations: nil)!
            context.drawRadialGradient(gradient, startCenter: p, startRadius: 0, endCenter: p,
                                       endRadius: radius, options: [])
        }
        context.restoreGState()
    }
    for tick in ticks(xDomain) {
        let value = x(tick)
        line(CGPoint(x: value, y: plot.minY), CGPoint(x: value, y: plot.maxY), grid)
        text(tickLabel(tick, axis: chart.xAxis), value, plot.maxY + 6, 17, soft, align: 0.5)
    }
    for tick in ticks(yDomain) {
        let value = y(tick)
        line(CGPoint(x: plot.minX, y: value), CGPoint(x: plot.maxX, y: value), grid)
        text(tickLabel(tick, axis: chart.yAxis), plot.minX - 10, value - 10, 17, soft, align: 1)
    }
    line(CGPoint(x: midX, y: plot.minY), CGPoint(x: midX, y: plot.maxY), guide, dashed: true)
    line(CGPoint(x: plot.minX, y: midY), CGPoint(x: plot.maxX, y: midY), guide, dashed: true)
    for connection in chart.connections ?? [] {
        guard connection.pointIndices.count >= 2,
              connection.pointIndices.allSatisfy(chart.points.indices.contains) else {
            throw ChartError(message: "折线须引用至少两个有效的点位索引（从 0 开始）")
        }
        guard let tint = hexColor(connection.color) else { throw ChartError(message: "折线颜色须为 #RRGGBB") }
        let vertices = connection.pointIndices.map { position(chart.points[$0]) }
        // 先画折线，再按原顺序绘制头像和队标。
        context.saveGState()
        context.setStrokeColor(tint.cgColor)
        context.setLineWidth(1.6)
        context.setLineCap(.round)
        context.setLineJoin(.round)
        context.addLines(between: vertices)
        context.strokePath()
        context.restoreGState()
    }
    for (point, logo) in zip(chart.points, logos) {
        let p = position(point)
        let halfHeight: Double
        if let logo {
            let factor = logoSize / max(logo.size.width, logo.size.height)
            let w = logo.size.width * factor, h = logo.size.height * factor
            halfHeight = h / 2
            logo.draw(in: CGRect(x: p.x - w / 2, y: p.y - halfHeight, width: w, height: h),
                      from: .zero, operation: .sourceOver, fraction: 1, respectFlipped: true,
                      hints: [.interpolation: NSImageInterpolation.high])
        } else {
            halfHeight = dotSize / 2
            context.setFillColor(ink.cgColor)
            context.fillEllipse(in: CGRect(x: p.x - halfHeight, y: p.y - halfHeight,
                                           width: dotSize, height: dotSize))
        }
        if let label = point.label {
            guard let tint = hexColor(label.color) else { throw ChartError(message: "点位标注颜色须为 #RRGGBB") }
            let gap = halfHeight + (label.gap ?? 7)
            let above = label.side == .above
            text(label.text, p.x + (label.offsetX ?? 0), p.y + (above ? -gap : gap), 17, tint,
                 bold: true, align: 0.5, verticalAlign: above ? 1 : 0)
        }
    }
    let neutral = color(100, 100, 100)
    let corners: [(String, Double, Double, Double, NSColor, Bool)] = [
        ("topLeft", plot.minX + 23, plot.minY + 25, 0, neutral, false),
        ("topRight", plot.maxX - 23, plot.minY + 25, 1, color(37, 92, 81), true),
        ("bottomLeft", plot.minX + 23, plot.maxY - 54, 0, color(133, 70, 83), true),
        ("bottomRight", plot.maxX - 23, plot.maxY - 54, 1, neutral, false)
    ]
    for (key, x, y, alignment, fill, bold) in corners {
        if let label = quadrants[key] {
            let offset = chart.quadrantOffsetsX?[key] ?? 0
            text(label, x + offset, y, 28, quadrantColors[key] ?? fill, bold: bold, align: alignment)
        }
    }
    text(chart.title, 40, 25, titleSize, titleColor, bold: titleBold, maxWidth: width - 80)
    if let subtitle = chart.subtitle {
        text(subtitle, 40, 91, subtitleSize, subtitleColor, bold: subtitleBold, maxWidth: width - 80,
             emphasis: chart.subtitleEmphasis)
    }
    text(xTitle, plot.midX, plot.maxY + 44, 20, ink, bold: true, align: 0.5, maxWidth: plot.width)
    context.saveGState()
    context.translateBy(x: 35, y: plot.midY)
    context.rotate(by: -.pi / 2)
    text(yTitle, 0, 0, 20, ink, bold: true, align: 0.5, maxWidth: plot.height)
    context.restoreGState()

    try FileManager.default.createDirectory(at: output.deletingLastPathComponent(), withIntermediateDirectories: true)
    guard let png = bitmap.representation(using: .png, properties: [:]) else {
        throw ChartError(message: "无法编码 PNG")
    }
    try png.write(to: output, options: .atomic)
    print("已保存 \(output.path)（3200×2400）")
}

let epmPadding = 0.10
let epmStyle: [String: Any] = ["axisPadding": epmPadding, "title": 48.0, "subtitle": 22.0,
                               "titleBold": true, "subtitleBold": true,
                               "titleColor": "#2C3E50", "subtitleColor": "#566473"]

struct Player {
    let id: Int
    let minutes: Double
    let offense: Double
    let defense: Double
    let team: String
}

let datasetFields: Set<String> = ["season", "seasontype", "scope_id", "team_alias", "player_id", "mp", "off", "def"]

func workbookRows(_ workbook: URL) throws -> [[String: String]] {
    func xml(_ member: String) throws -> XMLDocument {
        let process = Process(), pipe = Pipe()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/unzip")
        process.arguments = ["-p", workbook.path, member]
        process.standardOutput = pipe
        process.standardError = FileHandle.nullDevice
        try process.run()
        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        process.waitUntilExit()
        guard process.terminationStatus == 0 else { throw ChartError(message: "无法读取 \(workbook.lastPathComponent) 中的 \(member)") }
        return try XMLDocument(data: data, options: [.nodeLoadExternalEntitiesNever])
    }
    let sheets = try xml("xl/workbook.xml")
    let links = try xml("xl/_rels/workbook.xml.rels")
    let worksheets = try sheets.nodes(forXPath: "//*[local-name()='sheet']").compactMap { $0 as? XMLElement }
    guard let sheet = worksheets.first(where: { $0.attribute(forName: "name")?.stringValue == "实际 EPM" }) ?? worksheets.first,
          let id = sheet.attributes?.first(where: { $0.localName == "id" })?.stringValue,
          let relationship = try links.nodes(forXPath: "//*[local-name()='Relationship']").compactMap({ $0 as? XMLElement })
            .first(where: { $0.attribute(forName: "Id")?.stringValue == id }),
          let target = relationship.attribute(forName: "Target")?.stringValue else {
        throw ChartError(message: "工作簿缺少「实际 EPM」工作表")
    }
    let member = target.hasPrefix("/") ? String(target.dropFirst()) : "xl/" + target
    let document = try xml(member)
    let rows = try document.nodes(forXPath: "//*[local-name()='sheetData']/*[local-name()='row']")
    guard let header = rows.first as? XMLElement else { throw ChartError(message: "工作表为空") }
    var sharedStrings: [String]?
    func value(_ cell: XMLElement) throws -> String {
        let raw = cell.elements(forName: "v").first?.stringValue ?? ""
        switch cell.attribute(forName: "t")?.stringValue {
        case "inlineStr": return cell.elements(forName: "is").first?.stringValue ?? ""
        case "s":
            if sharedStrings == nil {
                sharedStrings = try xml("xl/sharedStrings.xml").rootElement()?.elements(forName: "si").map { $0.stringValue ?? "" }
            }
            guard let strings = sharedStrings, let i = Int(raw), strings.indices.contains(i) else {
                throw ChartError(message: "工作表的字符串索引无效")
            }
            return strings[i]
        default: return raw
        }
    }
    func column(_ cell: XMLElement) -> String {
        String((cell.attribute(forName: "r")?.stringValue ?? "").prefix(while: \.isLetter))
    }
    var fields = [String: String]()
    for cell in header.elements(forName: "c") {
        let name = try value(cell)
        if datasetFields.contains(name) { fields[column(cell)] = name }
    }
    guard Set(fields.values) == datasetFields else { throw ChartError(message: "工作表缺少 EPM 数据列") }
    return try rows.dropFirst().compactMap { node in
        guard let row = node as? XMLElement else { return nil }
        var values = [String: String]()
        for cell in row.elements(forName: "c") {
            if let name = fields[column(cell)] { values[name] = try value(cell) }
        }
        return values
    }
}

func readDataset(_ source: URL) throws -> [Player] {
    let rows: [[String: String]]
    switch source.pathExtension.lowercased() {
    case "xlsx":
        rows = try workbookRows(source)
    case "json":
        let data = try JSONSerialization.jsonObject(with: Data(contentsOf: source), options: [.fragmentsAllowed])
        let object = data as? [String: Any] ?? [:]
        guard let records = data as? [Any] ?? ["players", "data", "stats"].lazy.compactMap({ object[$0] as? [Any] }).first else {
            throw ChartError(message: "JSON 须包含 EPM 球员记录数组")
        }
        rows = records.compactMap { record in
            guard let record = record as? [String: Any] else { return nil }
            return record.filter { datasetFields.contains($0.key) }.compactMapValues { value in
                if let string = value as? String { return string }
                if let number = value as? NSNumber, CFGetTypeID(number) != CFBooleanGetTypeID() {
                    return number.stringValue
                }
                return nil
            }
        }
    default:
        throw ChartError(message: "数据文件须为 XLSX 或 JSON")
    }
    let players: [Player] = rows.compactMap { values in
        guard values["season"] == "2027", values["seasontype"] == "2", values["scope_id"] == "0",
              let team = values["team_alias"],
              let id = Int(values["player_id"] ?? ""), let mp = Double(values["mp"] ?? ""), mp.isFinite,
              let off = Double(values["off"] ?? ""), off.isFinite,
              let def = Double(values["def"] ?? ""), def.isFinite else { return nil }
        return Player(id: id, minutes: mp, offense: off, defense: def, team: team)
    }
    guard !players.isEmpty else { throw ChartError(message: "缺少 2026-27 常规赛有效的 EPM 球员记录") }
    return players
}

func findDataset(in directory: URL) throws -> (URL, [Player]) {
    let keys: Set<URLResourceKey> = [.contentModificationDateKey, .isRegularFileKey]
    let names = try FileManager.default.contentsOfDirectory(at: directory, includingPropertiesForKeys: Array(keys),
                                                            options: .skipsHiddenFiles)
    let candidates = try names.compactMap { url -> (url: URL, date: Date, isXLSX: Bool)? in
        let ext = url.pathExtension.lowercased()
        guard (ext == "xlsx" || ext == "json"), !url.lastPathComponent.hasPrefix("~$") else { return nil }
        let values = try url.resourceValues(forKeys: keys)
        guard values.isRegularFile == true else { return nil }
        return (url, values.contentModificationDate ?? .distantPast, ext == "xlsx")
    }.sorted { a, b in
        if a.date != b.date { return a.date > b.date }
        if a.isXLSX != b.isXLSX { return a.isXLSX }
        return a.url.lastPathComponent < b.url.lastPathComponent
    }
    var failures = [String]()
    for candidate in candidates {
        do {
            return try (candidate.url, readDataset(candidate.url))
        } catch {
            failures.append("\(candidate.url.lastPathComponent)（\(error.localizedDescription)）")
        }
    }
    let details = failures.isEmpty ? "未找到 XLSX 或 JSON 文件" : failures.joined(separator: "；")
    throw ChartError(message: "同目录没有可用的 EPM 数据集：\(details)")
}

func buildChart(_ dataset: [Player], team: String, root: URL) throws -> Chart {
    var players = team == "ALL" ? dataset : dataset.filter { $0.team == team }
    guard !players.isEmpty else { throw ChartError(message: "\(team) 没有具备攻防 EPM 的球员") }
    if team == "ALL" {
        players.sort { $0.minutes == $1.minutes ? $0.id < $1.id : $0.minutes > $1.minutes }
        players = Array(players.prefix((players.count + 3) / 4))
    }
    let points = try players.sorted { $0.defense > $1.defense }.map { player in
        for folder in ["球员组合图", "头像", "素材/球员组合图", "素材/头像"] {
            for ext in ["webp", "png"] {
                let path = root.appendingPathComponent("\(folder)/\(player.id).\(ext)").path
                if FileManager.default.fileExists(atPath: path) {
                    return Point(x: player.offense, y: player.defense, logo: path, label: nil)
                }
            }
        }
        throw ChartError(message: "缺少球员头像：\(player.id)")
    }
    func axis(_ label: String, _ values: [Double]) throws -> Axis {
        let auto = Axis(label: label, low: nil, high: nil, reverse: nil, limits: nil, percent: nil)
        let range = try domain(auto, values, paddingRatio: epmPadding)
        let pad = (range.upperBound - range.lowerBound) * 0.05
        return Axis(label: label, low: nil, high: nil, reverse: nil,
                    limits: [min(range.lowerBound, -pad), max(range.upperBound, pad)], percent: nil)
    }
    let red = "#C5352B", paper = "#F2F3F4", green = "#2B884D"
    return try Chart(title: "NBA2026-27常规赛球员EPM攻防散点图", subtitle: "2026/11/27｜制作：俗人罢了｜Ducks&Threes",
                     subtitleEmphasis: nil, xAxis: axis("进攻偏弱 ← 进攻 EPM → 进攻偏强", points.map(\.x)),
                     yAxis: axis("防守偏弱 ← 防守 EPM → 防守偏强", points.map(\.y)), points: points,
                     connections: nil, quadrants: ["bottomLeft": "攻防拉完", "topRight": "攻防一体"],
                     quadrantColors: ["bottomLeft": red, "topRight": green], quadrantOffsetsX: nil,
                     backgroundColor: paper, backgroundImage: nil, reference: .zero,
                     logoSize: 100, dotSize: nil, glow: false, glowPalette: nil)
}
do {
    let root = URL(fileURLWithPath: #filePath).standardizedFileURL.deletingLastPathComponent()
    var args = Array(CommandLine.arguments.dropFirst())
    var team = "ALL"
    if let i = args.firstIndex(of: "--team") {
        guard i + 1 < args.count else { throw ChartError(message: "--team 后须填写球队缩写") }
        team = args[i + 1].uppercased()
        args.removeSubrange(i...i + 1)
    }
    guard args.count <= 2 else { throw ChartError(message: "用法：swift 生图.swift [数据.xlsx 或 数据.json] [成品.png] [--team LAL]") }
    let input: URL, players: [Player]
    if let path = args.first {
        input = URL(fileURLWithPath: path).standardizedFileURL
        players = try readDataset(input)
    } else {
        (input, players) = try findDataset(in: root)
    }
    let name = team == "ALL" ? "EPM攻防图" : "EPM攻防图_\(team)"
    let output = args.count == 2 ? URL(fileURLWithPath: args[1]).standardizedFileURL
        : root.appendingPathComponent("\(name).png")
    guard output.pathExtension.lowercased() == "png",
          output.resolvingSymlinksInPath() != input.resolvingSymlinksInPath() else {
        throw ChartError(message: "请指定独立的 PNG 输出路径")
    }
    let chart = try buildChart(players, team: team, root: root)
    print("使用数据：\(input.lastPathComponent)")
    try render(chart, input: input, output: output)
} catch {
    FileHandle.standardError.write(Data("绘图失败：\(error.localizedDescription)\n".utf8))
    exit(1)
}
