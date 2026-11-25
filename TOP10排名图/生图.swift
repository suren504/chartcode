// macOS：swift TOP10排名图/生图.swift
// 自动识别同目录的 GBPM JSON，也可在命令行指定一个数据文件。
import CoreGraphics
import CoreText
import Foundation
import ImageIO

struct PlayerData: Decodable {
    struct Metadata: Decodable { let generatedAt: String? }
    let meta: Metadata?
    let players: [Player]
}

struct Player: Decodable {
    struct Stats: Decodable {
        let gbpm: Double?
    }

    let playerId: Int
    let playerName: String
    let teamAbbreviation: String
    let stats: Stats?
}

struct TeamColor: Decodable {
    let team_abbreviation: String
    let team_color: String
}

struct ChartError: LocalizedError {
    let message: String
    var errorDescription: String? { message }
}

func color(_ rgb: UInt32, whiteMix: CGFloat = 0) -> CGColor {
    let r = CGFloat((rgb >> 16) & 255) / 255
    let g = CGFloat((rgb >> 8) & 255) / 255
    let b = CGFloat(rgb & 255) / 255
    return CGColor(srgbRed: r + (1 - r) * whiteMix,
                   green: g + (1 - g) * whiteMix,
                   blue: b + (1 - b) * whiteMix, alpha: 1)
}

let dpi = 300
let pixelWidth = 3600
let pixelHeight = 4800
let scale = CGFloat(dpi) / 72
let pageWidth = CGFloat(pixelWidth) / scale
let pageHeight = CGFloat(pixelHeight) / scale
let ink = color(0x000000)
let white = color(0xffffff)
let chineseBold = preferredFont("Microsoft YaHei Bold", fallback: "PingFangSC-Semibold")
let monoBold = preferredFont("Consolas Bold", fallback: "Menlo-Bold")
let metricFontSize: CGFloat = 39

func preferredFont(_ name: String, fallback: String) -> String {
    let font = CTFontCreateWithName(name as CFString, 12, nil)
    return (CTFontCopyFullName(font) as String == name) ? name : fallback
}

func rect(_ x: CGFloat, _ top: CGFloat, _ width: CGFloat, _ height: CGFloat) -> CGRect {
    CGRect(x: x, y: pageHeight - top - height, width: width, height: height)
}

typealias TextRun = (text: String, font: String, size: CGFloat, fill: CGColor)

func drawText(_ runs: [TextRun], on context: CGContext, x: CGFloat, y: CGFloat, outline: CGColor? = nil) {
    let text = NSMutableAttributedString()
    for run in runs {
        var attributes: [NSAttributedString.Key: Any] = [
            NSAttributedString.Key(kCTFontAttributeName as String): CTFontCreateWithName(run.font as CFString, run.size, nil),
            NSAttributedString.Key(kCTForegroundColorAttributeName as String): run.fill,
        ]
        if let outline {
            attributes[NSAttributedString.Key(kCTStrokeColorAttributeName as String)] = outline
            attributes[NSAttributedString.Key(kCTStrokeWidthAttributeName as String)] = -4.0
        }
        text.append(NSAttributedString(string: run.text, attributes: attributes))
    }
    let line = CTLineCreateWithAttributedString(text)
    let bounds = CTLineGetBoundsWithOptions(line, .useGlyphPathBounds)
    context.textPosition = CGPoint(x: x - bounds.midX, y: pageHeight - y - bounds.midY)
    CTLineDraw(line, context)
}

func loadImage(_ url: URL) throws -> CGImage {
    guard let source = CGImageSourceCreateWithURL(url as CFURL, nil),
          let image = CGImageSourceCreateImageAtIndex(source, 0, nil) else {
        throw ChartError(message: "无法读取素材 \(url.path)")
    }
    return image
}

func drawImage(_ image: CGImage, on context: CGContext, x: CGFloat, y: CGFloat,
               maxWidth: CGFloat, maxHeight: CGFloat, verticalAnchor: CGFloat = 0.5) {
    let scale = min(maxWidth / CGFloat(image.width), maxHeight / CGFloat(image.height))
    let width = CGFloat(image.width) * scale
    let height = CGFloat(image.height) * scale
    context.draw(image, in: rect(x - width / 2, y - height * verticalAnchor, width, height))
}

func playerData(in directory: URL) throws -> PlayerData {
    let decoder = JSONDecoder()
    decoder.keyDecodingStrategy = .convertFromSnakeCase
    if let path = CommandLine.arguments.dropFirst().first {
        return try decoder.decode(PlayerData.self, from: Data(contentsOf: URL(fileURLWithPath: path)))
    }
    let files = try FileManager.default.contentsOfDirectory(at: directory, includingPropertiesForKeys: nil)
    let matches = files.filter { $0.pathExtension.lowercased() == "json" && !["teamcolor.json", "id查询.json"].contains($0.lastPathComponent) }.compactMap { url -> PlayerData? in
        guard let bytes = try? Data(contentsOf: url), let data = try? decoder.decode(PlayerData.self, from: bytes),
              data.players.contains(where: { $0.stats?.gbpm != nil }) else { return nil }
        return data
    }
    guard matches.count == 1 else { throw ChartError(message: "请在脚本旁放一个含 GBPM 的球员 JSON，或运行时指定文件路径") }
    return matches[0]
}

func headshot(id: Int, cache: URL) throws -> URL {
    let file = cache.appendingPathComponent("\(id).png")
    if FileManager.default.fileExists(atPath: file.path), (try? loadImage(file)) != nil { return file }
    let remote = URL(string: "https://cdn.nba.com/headshots/nba/latest/1040x760/\(id).png")!
    let bytes = try Data(contentsOf: remote)
    guard let source = CGImageSourceCreateWithData(bytes as CFData, nil),
          CGImageSourceCreateImageAtIndex(source, 0, nil) != nil else { throw ChartError(message: "头像下载无效：\(id)") }
    try bytes.write(to: file, options: .atomic)
    return file
}

func render(directory: URL) throws {
    let data = try playerData(in: directory)
    let players = data.players.compactMap { player -> (player: Player, gbpm: Double)? in
        guard let gbpm = player.stats?.gbpm, gbpm.isFinite else { return nil }
        return (player, gbpm)
    }.sorted { lhs, rhs in
        if lhs.gbpm == rhs.gbpm { return lhs.player.playerId < rhs.player.playerId }
        return lhs.gbpm > rhs.gbpm
    }.prefix(10)
    guard !players.isEmpty else { throw ChartError(message: "数据中没有有效的 GBPM") }

    let portraits = directory.appendingPathComponent("头像")
    try FileManager.default.createDirectory(at: portraits, withIntermediateDirectories: true)
    let logos = directory.appendingPathComponent("LOGO")
    let teamData = try Data(contentsOf: directory.appendingPathComponent("teamcolor.json"))
    let teamColors = try JSONDecoder().decode([TeamColor].self, from: teamData)
    let palette = Dictionary(teamColors.map { ($0.team_abbreviation, $0.team_color) }, uniquingKeysWith: { first, _ in first })

    guard let space = CGColorSpace(name: CGColorSpace.sRGB),
          let context = CGContext(data: nil, width: pixelWidth, height: pixelHeight, bitsPerComponent: 8,
                                  bytesPerRow: 0, space: space, bitmapInfo: CGImageAlphaInfo.noneSkipLast.rawValue) else {
        throw ChartError(message: "无法创建绘图画布")
    }
    context.scaleBy(x: scale, y: scale)
    context.interpolationQuality = .high
    context.setFillColor(color(0xf2f3f4))
    context.fill(rect(0, 0, pageWidth, pageHeight))

    drawText([("NBA 球员 GBPM TOP 10", chineseBold, 32, ink)], on: context, x: pageWidth / 2, y: 39)
    let muted = color(0x555555)
    var subtitle: [TextRun] = [
        ("数据来源 PBP Stats｜制图 ", chineseBold, 16, muted),
        ("俗人罢了", chineseBold, 18, ink),
    ]
    if let raw = data.meta?.generatedAt, let date = ISO8601DateFormatter().date(from: raw) {
        let format = DateFormatter()
        format.locale = Locale(identifier: "en_US_POSIX")
        format.timeZone = TimeZone(identifier: "Asia/Shanghai")
        format.dateFormat = "yyyy/MM/dd"
        subtitle.append(("｜数据更新至 " + format.string(from: date), chineseBold, 16, muted))
    }
    drawText(subtitle, on: context, x: pageWidth / 2, y: 78)

    let rankX: CGFloat = 85
    let logoX: CGFloat = 165
    let logoSize: CGFloat = 76
    let portraitX: CGFloat = 250
    let portraitWidth: CGFloat = 128
    let portraitHeight: CGFloat = 94
    let left: CGFloat = 39
    let right: CGFloat = 825
    let metricWidth: CGFloat = 128
    let borderWidth: CGFloat = 4
    let divider = right - metricWidth
    let metricX = divider + metricWidth / 2
    let nameX = (portraitX + portraitWidth / 2 + divider) / 2
    let headings = [("排名", rankX), ("球队", logoX), ("靓照", portraitX), ("球员", nameX), ("GBPM", metricX)]
    for (label, x) in headings {
        drawText([(label, chineseBold, 22, ink)], on: context, x: x, y: 128)
    }

    var loadedLogos: [String: CGImage] = [:]
    var portraitPlacements: [(url: URL, bottom: CGFloat)] = []
    context.setLineWidth(borderWidth)
    for (index, entry) in players.enumerated() {
        let player = entry.player
        let team = player.teamAbbreviation
        guard let hex = palette[team], hex.count == 7, hex.hasPrefix("#"),
              let rgb = UInt32(hex.dropFirst(), radix: 16) else {
            throw ChartError(message: "缺少有效的 \(team) 球队主色")
        }
        let logo = try loadedLogos[team] ?? loadImage(logos.appendingPathComponent("\(team).webp"))
        loadedLogos[team] = logo

        let y = 200 + CGFloat(index) * 98
        let playerBox = rect(left, y - 40, divider - left, 80)
        let metricBox = rect(divider, y - 45, metricWidth, 90)
        let portraitURL = try headshot(id: player.playerId, cache: portraits)
        portraitPlacements.append((portraitURL, pageHeight - playerBox.minY - borderWidth / 2))
        context.setFillColor(color(rgb, whiteMix: 0.93))
        context.fill(playerBox)
        context.setFillColor(color(rgb, whiteMix: 0.45))
        context.fill(rect(divider + 6, y - 40, metricWidth - 12, 80))
        drawImage(logo, on: context, x: logoX, y: y, maxWidth: logoSize, maxHeight: logoSize)
        context.setStrokeColor(color(rgb))
        context.stroke(playerBox)
        context.stroke(metricBox)

        drawText([("#\(index + 1)", monoBold, 31, ink)], on: context, x: rankX, y: y)
        drawText([(player.playerName, monoBold, 25, ink)], on: context, x: nameX, y: y)
        let gbpmLabel = String(format: "%.2f", entry.gbpm)
        drawText([(gbpmLabel, monoBold, metricFontSize, white)], on: context, x: metricX, y: y, outline: ink)
    }

    // 头像最后绘制，越过框线的部分保留在最上层。
    for placement in portraitPlacements {
        let portrait = try loadImage(placement.url)
        drawImage(portrait, on: context, x: portraitX, y: placement.bottom,
                  maxWidth: portraitWidth, maxHeight: portraitHeight, verticalAnchor: 1)
    }

    let output = directory.appendingPathComponent("NBA_plot.png")
    let png = NSMutableData()
    guard let image = context.makeImage(),
          let destination = CGImageDestinationCreateWithData(png, "public.png" as CFString, 1, nil) else {
        throw ChartError(message: "无法创建 PNG")
    }
    CGImageDestinationAddImage(destination, image, [kCGImagePropertyDPIWidth: dpi, kCGImagePropertyDPIHeight: dpi] as CFDictionary)
    guard CGImageDestinationFinalize(destination) else { throw ChartError(message: "无法编码 PNG") }
    try (png as Data).write(to: output, options: .atomic)
    print("已保存 \(output.path)（\(pixelWidth) × \(pixelHeight)，\(players.count) 位球员）")
}

do {
    let directory = URL(fileURLWithPath: #filePath).standardizedFileURL.deletingLastPathComponent()
    try render(directory: directory)
} catch {
    FileHandle.standardError.write(Data("绘图失败：\(error.localizedDescription)\n".utf8))
    exit(1)
}
