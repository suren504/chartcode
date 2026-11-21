// macOS 运行方式（保留本目录结构）
// swift 生成.swift                                  使用目录内的示例数据和日期
// swift 生成.swift 2026-11-21                       使用目录内的数据并指定日期
// swift 生成.swift 新数据.json 2026-11-21            使用JSON 并指定日期
// JSON 为 30 支球队的数组，每项提供 team（缩写）、offRtg、defRtg。
// 素材和输出均以脚本所在目录为基准。每次生成 2000 × 2500 的 PNG。

import CoreGraphics
import CoreText
import Foundation
import ImageIO

struct ChartError: LocalizedError {
    let errorDescription: String?

    init(_ message: String) {
        errorDescription = message
    }
}

let logoSizes: [String: CGFloat] = [
    "ATL": 106, "BKN": 96, "BOS": 120, "CHA": 114, "CHI": 114, "CLE": 114,
    "DAL": 96, "DEN": 96, "DET": 96, "GSW": 96, "HOU": 150, "IND": 105,
    "LAC": 107, "LAL": 111, "MEM": 102, "MIA": 113, "MIL": 125, "MIN": 95,
    "NOP": 120, "NYK": 122, "OKC": 144, "ORL": 145, "PHI": 121, "PHX": 125,
    "POR": 104, "SAC": 95, "SAS": 123, "TOR": 106, "UTA": 155, "WAS": 94,
]

struct Point: Decodable {
    let team: String
    let offRtg: CGFloat
    let defRtg: CGFloat
}

func readPoints(_ source: URL) throws -> [Point] {
    let points: [Point]
    do {
        points = try JSONDecoder().decode([Point].self, from: Data(contentsOf: source))
    } catch let error as DecodingError {
        throw ChartError("JSON 须为球队数组，每项含 team、offRtg、defRtg：\(error)")
    }
    var seen = Set<String>()
    for point in points {
        guard logoSizes[point.team] != nil else { throw ChartError("无法识别球队缩写：\(point.team)") }
        guard seen.insert(point.team).inserted else { throw ChartError("球队重复：\(point.team)") }
        guard point.offRtg.isFinite && point.offRtg > 0, point.defRtg.isFinite && point.defRtg > 0 else {
            throw ChartError("\(point.team) 的攻防效率须为正数")
        }
    }
    let missing = Set(logoSizes.keys).subtracting(seen)
    guard missing.isEmpty else { throw ChartError("数据缺少球队：\(missing.sorted().joined(separator: "、"))") }
    return points
}

let width: CGFloat = 2000
let height: CGFloat = 2500
let plotTop = height - width
let colorSpace = CGColorSpace(name: CGColorSpace.sRGB)!
let rgba = CGImageAlphaInfo.premultipliedLast.rawValue | CGBitmapInfo.byteOrder32Big.rawValue

func color(_ rgb: UInt32) -> CGColor {
    let r = CGFloat((rgb >> 16) & 255) / 255
    let g = CGFloat((rgb >> 8) & 255) / 255
    let b = CGFloat(rgb & 255) / 255
    return CGColor(srgbRed: r, green: g, blue: b, alpha: 1)
}

let ink = color(0x171717)
let muted = color(0x625e56)

func loadImage(_ url: URL, trim: Bool = false) throws -> CGImage {
    guard let source = CGImageSourceCreateWithURL(url as CFURL, nil),
          let image = CGImageSourceCreateImageAtIndex(source, 0, nil) else {
        throw ChartError("无法读取素材：\(url.lastPathComponent)")
    }
    guard trim else { return image }

    let w = image.width, h = image.height
    guard let pixels = CGContext(data: nil, width: w, height: h, bitsPerComponent: 8,
                                 bytesPerRow: w * 4, space: colorSpace, bitmapInfo: rgba),
          let bytes = pixels.data?.assumingMemoryBound(to: UInt8.self) else {
        throw ChartError("无法处理素材：\(url.lastPathComponent)")
    }
    pixels.draw(image, in: CGRect(x: 0, y: 0, width: w, height: h))
    var left = w, right = -1, top = h, bottom = -1
    for y in 0..<h {
        for x in 0..<w where bytes[(y * w + x) * 4 + 3] > 0 {
            left = min(left, x)
            right = max(right, x)
            top = min(top, y)
            bottom = max(bottom, y)
        }
    }
    guard right >= left, bottom >= top,
          let cropped = image.cropping(to: CGRect(x: left, y: top, width: right - left + 1, height: bottom - top + 1)) else {
        throw ChartError("素材没有可见内容：\(url.lastPathComponent)")
    }
    return cropped
}

struct Lettering {
    let line: CTLine
    let bounds: CGRect

    init(_ runs: [(String, String, CGFloat, CGColor)]) {
        let text = NSMutableAttributedString()
        for (content, font, size, fill) in runs {
            let attributes: [NSAttributedString.Key: Any] = [
                NSAttributedString.Key(kCTFontAttributeName as String): CTFontCreateWithName(font as CFString, size, nil),
                NSAttributedString.Key(kCTForegroundColorAttributeName as String): fill,
            ]
            text.append(NSAttributedString(string: content, attributes: attributes))
        }
        line = CTLineCreateWithAttributedString(text)
        bounds = CTLineGetBoundsWithOptions(line, .useGlyphPathBounds)
    }

    func draw(_ context: CGContext, x: CGFloat, y: CGFloat, angle: CGFloat = 0) {
        context.saveGState()
        context.translateBy(x: x, y: height - y)
        context.rotate(by: angle)
        context.textMatrix = .identity
        context.textPosition = CGPoint(x: -bounds.minX, y: -bounds.maxY)
        CTLineDraw(line, context)
        context.restoreGState()
    }
}

func drawImage(_ image: CGImage, on context: CGContext, x: CGFloat, y: CGFloat, w: CGFloat, h: CGFloat) {
    context.draw(image, in: CGRect(x: x, y: height - y - h, width: w, height: h))
}

func drawHeading(_ context: CGContext, nbaMark: CGImage, date: String) {
    let title = Lettering([("The Efficiency Landscape", "AvenirNext-Heavy", 128, ink)])
    let titleTop: CGFloat = 52
    let subtitleTop = titleTop + 153
    title.draw(context, x: 227, y: titleTop)
    let sans = "AvenirNext-Regular"
    let chinese = "HiraginoSansGB-W3"
    let subtitle = Lettering([
        ("\(date) | ", sans, 43, muted),
        ("Last 10 Games Only", sans, 43, color(0xcf4d48)),
        (" | By @", sans, 43, muted),
        ("俗人罢了", chinese, 43, muted),
    ])
    subtitle.draw(context, x: (width - subtitle.bounds.width) / 2, y: subtitleTop)
    let markHeight = subtitleTop + subtitle.bounds.height - titleTop
    let markWidth = markHeight * CGFloat(nbaMark.width) / CGFloat(nbaMark.height)
    drawImage(nbaMark, on: context, x: 73, y: titleTop, w: markWidth, h: markHeight)
}

func drawGuides(_ context: CGContext, origin: CGPoint) {
    let axisLeft: CGFloat = 33, axisRight: CGFloat = 1967
    let axisTop = plotTop + 20, axisBottom: CGFloat = 2466
    func screen(_ x: CGFloat, _ y: CGFloat) -> CGPoint { CGPoint(x: x, y: height - y) }

    // 横纵轴使用相同的尺度，因此 OffRtg = DefRtg 是一条 45° 线。
    let intercept = origin.y - origin.x
    let start = max(126, plotTop + 30 - intercept)
    let end = min(1910, axisBottom - intercept)
    context.saveGState()
    context.setStrokeColor(color(0xa29f97))
    context.setLineWidth(4)
    context.setLineDash(phase: 0, lengths: [14, 10])
    context.strokeLineSegments(between: [screen(start, start + intercept), screen(end, end + intercept)])
    context.restoreGState()

    let sans = "HiraginoSansGB-W3"
    let strong = Lettering([("联盟强队", sans, 32, color(0x89857c))])
    let weak = Lettering([("联盟弱队", sans, 32, color(0x89857c))])
    let labelX = start + 32, labelY = labelX + intercept
    let above = (strong.bounds.height + 10) / sqrt(2.0)
    let below = 10 / sqrt(2.0)
    strong.draw(context, x: labelX + above, y: labelY - above, angle: -.pi / 4)
    weak.draw(context, x: labelX - below, y: labelY + below, angle: -.pi / 4)

    context.setStrokeColor(ink)
    context.setFillColor(ink)
    context.setLineWidth(2)
    context.strokeLineSegments(between: [
        screen(axisLeft + 16, origin.y), screen(axisRight - 16, origin.y),
        screen(origin.x, axisTop + 16), screen(origin.x, axisBottom - 16),
    ])
    func arrow(_ tip: CGPoint, _ side: CGPoint, _ other: CGPoint) {
        context.beginPath()
        context.addLines(between: [tip, side, other])
        context.closePath()
        context.fillPath()
    }
    arrow(screen(axisLeft, origin.y), screen(axisLeft + 17, origin.y - 7), screen(axisLeft + 17, origin.y + 7))
    arrow(screen(axisRight, origin.y), screen(axisRight - 17, origin.y - 7), screen(axisRight - 17, origin.y + 7))
    arrow(screen(origin.x, axisTop), screen(origin.x - 7, axisTop + 17), screen(origin.x + 7, axisTop + 17))
    arrow(screen(origin.x, axisBottom), screen(origin.x - 7, axisBottom - 17), screen(origin.x + 7, axisBottom - 17))

    let offense = Lettering([("进攻效率", sans, 55, ink)])
    let defense = Lettering([("防守效率", sans, 55, ink)])
    offense.draw(context, x: 1934 - offense.bounds.width, y: origin.y + 12)
    defense.draw(context, x: origin.x - 11 - defense.bounds.height,
                 y: axisTop + 25 + defense.bounds.width, angle: .pi / 2)
}

func render(_ points: [Point], directory: URL, date: String, output: URL) throws {
    let logoDirectory = directory.appendingPathComponent("LOGO", isDirectory: true)
    let nbaMark = try loadImage(logoDirectory.appendingPathComponent("nba-mark.png"), trim: true)
    let paper = try loadImage(directory.appendingPathComponent("粗纹纸张.png"))
    guard let context = CGContext(data: nil, width: Int(width), height: Int(height), bitsPerComponent: 8,
                                  bytesPerRow: 0, space: colorSpace,
                                  bitmapInfo: CGImageAlphaInfo.noneSkipLast.rawValue) else {
        throw ChartError("无法创建画布")
    }
    context.interpolationQuality = .high
    let paperScale = max(width / CGFloat(paper.width), height / CGFloat(paper.height))
    let paperWidth = CGFloat(paper.width) * paperScale, paperHeight = CGFloat(paper.height) * paperScale
    drawImage(paper, on: context, x: (width - paperWidth) / 2, y: (height - paperHeight) / 2,
              w: paperWidth, h: paperHeight)
    drawHeading(context, nbaMark: nbaMark, date: date)

    let offenses = points.map(\.offRtg), defenses = points.map(\.defRtg)
    let average = (offenses.reduce(0, +) + defenses.reduce(0, +)) / CGFloat(2 * points.count)
    let offMin = min(offenses.min()!, average), offMax = max(offenses.max()!, average)
    let defMin = min(defenses.min()!, average), defMax = max(defenses.max()!, average)
    let longestSpan = max(offMax - offMin, defMax - defMin, 1)
    let scale = (width - 190) / longestSpan
    let units = scale >= 1 ? floor(scale) : scale

    func position(_ offense: CGFloat, _ defense: CGFloat) -> CGPoint {
        CGPoint(x: width / 2 + (offense - (offMin + offMax) / 2) * units,
                y: (plotTop + height) / 2 + (defense - (defMin + defMax) / 2) * units)
    }
    drawGuides(context, origin: position(average, average))

    // 沿用原图的队标尺寸，保留数据原位及重叠。
    var marks: [String: CGRect] = [:]
    for point in points.reversed() {
        let mark = try loadImage(logoDirectory.appendingPathComponent("\(point.team).webp"), trim: true)
        let ratio = logoSizes[point.team]! / CGFloat(max(mark.width, mark.height))
        let w = CGFloat(mark.width) * ratio, h = CGFloat(mark.height) * ratio
        let center = position(point.offRtg, point.defRtg)
        marks[point.team] = CGRect(x: center.x - w / 2, y: center.y - h / 2, width: w, height: h)
        drawImage(mark, on: context, x: center.x - w / 2, y: center.y - h / 2, w: w, h: h)
    }

    struct TeamColor: Decodable {
        let team_abbreviation: String
        let team_color: String
    }
    let palette = try JSONDecoder().decode([TeamColor].self, from: Data(contentsOf: directory.appendingPathComponent("teamcolor.json")))
    func net(_ point: Point) -> CGFloat { ((point.offRtg - point.defRtg) * 10).rounded() }
    let nets = points.map(net)
    let bestNet = nets.max()!, worstNet = nets.min()!
    let bestOff = offenses.max()!, worstOff = offenses.min()!
    let bestDef = defenses.min()!, worstDef = defenses.max()!
    let origin = position(average, average)
    var occupied = Array(marks.values)
    occupied.append(CGRect(x: 1700, y: origin.y + 8, width: 240, height: 65))
    occupied.append(CGRect(x: origin.x - 75, y: plotTop + 40, width: 70, height: 235))
    let area = CGRect(x: 12, y: plotTop, width: width - 24, height: height - plotTop - 12)
    for point in points {
        var labels: [String] = []
        if point.offRtg == bestOff { labels.append("最佳进攻") }
        if point.offRtg == worstOff { labels.append("最差进攻") }
        if point.defRtg == bestDef { labels.append("最佳防守") }
        if point.defRtg == worstDef { labels.append("最差防守") }
        if net(point) == bestNet || net(point) == worstNet {
            if labels.isEmpty {
                labels.append("#联盟第\(1 + nets.filter { $0 > net(point) }.count)")
            }
            labels.append("#进攻第\(1 + offenses.filter { $0 > point.offRtg }.count)")
            labels.append("#防守第\(1 + defenses.filter { $0 < point.defRtg }.count)")
        }
        guard !labels.isEmpty else { continue }
        guard let hex = palette.first(where: { $0.team_abbreviation == point.team })?.team_color,
              hex.count == 7, hex.first == "#", let rgb = UInt32(hex.dropFirst(), radix: 16) else {
            throw ChartError("teamcolor.json 缺少有效主色：\(point.team)")
        }
        let lines = labels.map { Lettering([($0, "HiraginoSansGB-W3", 29, color(rgb))]) }
        let w = ceil(lines.map { $0.bounds.width }.max()!)
        let h = CGFloat((lines.count - 1) * 34) + ceil(lines.last!.bounds.height)
        let mark = marks[point.team]!
        var best: CGRect?
        var bestCost = CGFloat.infinity
        for gap: CGFloat in [9, 26, 48, 76, 110] {
            for (dx, dy): (CGFloat, CGFloat) in [(1,0),(-1,0),(0,-1),(0,1),(1,-1),(-1,-1),(1,1),(-1,1)] {
                let x = mark.midX + dx * ((mark.width + w) / 2 + gap)
                let y = mark.midY + dy * ((mark.height + h) / 2 + gap)
                let rect = CGRect(x: x - w / 2, y: y - h / 2, width: w, height: h)
                guard area.contains(rect) else { continue }
                let overlap = occupied.reduce(CGFloat.zero) { total, obstacle in
                    let hit = rect.intersection(obstacle.insetBy(dx: -4, dy: -4))
                    return total + (hit.isNull ? 0 : hit.width * hit.height)
                }
                let cost = overlap * 1000 + hypot(x - mark.midX, y - mark.midY)
                if cost < bestCost { bestCost = cost; best = rect }
            }
        }
        guard let rect = best else { throw ChartError("无法放置排名：\(point.team)") }
        occupied.append(rect)
        for (index, line) in lines.enumerated() {
            line.draw(context, x: rect.minX, y: rect.minY + CGFloat(index * 34))
        }
    }

    let png = NSMutableData()
    guard let image = context.makeImage(),
          let encoder = CGImageDestinationCreateWithData(png, "public.png" as CFString, 1, nil) else {
        throw ChartError("无法创建 PNG")
    }
    CGImageDestinationAddImage(encoder, image, nil)
    guard CGImageDestinationFinalize(encoder) else { throw ChartError("无法编码 PNG") }
    try (png as Data).write(to: output, options: .atomic)
}

func run() throws {
    let directory = URL(fileURLWithPath: #filePath).standardizedFileURL.deletingLastPathComponent()
    let args = Array(CommandLine.arguments.dropFirst())
    let usage = "用法：swift 生成.swift [YYYY-MM-DD | 数据.json YYYY-MM-DD]"
    if args == ["--help"] || args == ["-h"] {
        print(usage)
        return
    }
    guard args.count <= 2 else { throw ChartError(usage) }

    let source = args.count == 2 ? URL(fileURLWithPath: args[0]).standardizedFileURL
        : directory.appendingPathComponent("过去10场攻防效率数据.json")
    let date = args.last ?? "2026-11-20"
    let formatter = DateFormatter()
    formatter.locale = Locale(identifier: "en_US_POSIX")
    formatter.timeZone = TimeZone(secondsFromGMT: 0)
    formatter.dateFormat = "yyyy-MM-dd"
    guard let parsed = formatter.date(from: date), formatter.string(from: parsed) == date else {
        throw ChartError("图表日期须为有效的 YYYY-MM-DD")
    }
    formatter.dateFormat = "MMM'.' d, yyyy"
    let output = directory.appendingPathComponent("过去10场攻防效率_\(date).png")
    let points = try readPoints(source)
    try render(points, directory: directory, date: formatter.string(from: parsed), output: output)
    print("已保存 \(output.path)（2000 × 2500，\(points.count) 支球队）")
}

do {
    try run()
} catch {
    FileHandle.standardError.write(Data("绘图失败：\(error.localizedDescription)\n".utf8))
    exit(1)
}
