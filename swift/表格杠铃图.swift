import AppKit
import Foundation
import CoreText

let root = URL(fileURLWithPath: #filePath).standardizedFileURL.deletingLastPathComponent().deletingLastPathComponent()
guard CommandLine.arguments.count > 1 else {
    print("用法：swift swift/表格杠铃图.swift 数据.json [成品.png]")
    exit(0)
}
let dataFile = URL(fileURLWithPath: CommandLine.arguments[1]).standardizedFileURL
let destination = CommandLine.arguments.count > 2
    ? URL(fileURLWithPath: CommandLine.arguments[2]).standardizedFileURL
    : root.appendingPathComponent("成品图/表格杠铃图.png")
let paperFile = destination.pathExtension.lowercased() == "png" && !destination.hasDirectoryPath
    ? destination : destination.appendingPathComponent("表格杠铃图.png")
let outputDirectory = paperFile.deletingLastPathComponent()
let transparentFile = outputDirectory.appendingPathComponent(paperFile.deletingPathExtension().lastPathComponent + "_透明.png")
guard [paperFile, transparentFile].allSatisfy({ $0.resolvingSymlinksInPath() != dataFile.resolvingSymlinksInPath() }) else {
    FileHandle.standardError.write(Data("请指定独立的 PNG 输出路径\n".utf8))
    exit(1)
}
struct Configuration: Decodable {
    let title: String, playerLabel: String
    let subtitle: String?
    let columnLabels: [String]
    let paperWhiteOverlay: Double?
    let rows: [Row]?
}
let decoder = JSONDecoder()
let input = try Data(contentsOf: dataFile)
let configuration: Configuration
let sourceRows: [Row]
if CommandLine.arguments.count <= 3,
   let combined = try? decoder.decode(Configuration.self, from: input), let rows = combined.rows {
    configuration = combined
    sourceRows = rows
} else {
    let configFile = CommandLine.arguments.count > 3
        ? URL(fileURLWithPath: CommandLine.arguments[3]).standardizedFileURL
        : root.appendingPathComponent("绘图配置.json")
    guard FileManager.default.fileExists(atPath: configFile.path) else {
        FileHandle.standardError.write(Data("输入须包含标题、表头和 rows，旧版数组需另传配置文件\n".utf8))
        exit(1)
    }
    configuration = try decoder.decode(Configuration.self, from: Data(contentsOf: configFile))
    sourceRows = try decoder.decode([Row].self, from: input)
}
let styles = try JSONSerialization.jsonObject(with: Data(contentsOf: root.appendingPathComponent("样式.json")))
    as? [String: [String: Any]] ?? [:]
let heading = (styles["header"] ?? [:]).merging(styles["表格杠铃图"] ?? [:]) { _, value in value }
func headingColor(_ key: String) -> NSColor? {
    guard let value = heading[key] as? String, let rgb = UInt32(value.dropFirst(), radix: 16) else { return nil }
    return NSColor(srgbRed: Double((rgb >> 16) & 255) / 255,
                   green: Double((rgb >> 8) & 255) / 255, blue: Double(rgb & 255) / 255, alpha: 1)
}
guard let titleSize = heading["title"] as? Double, let subtitleSize = heading["subtitle"] as? Double,
      let titleBold = heading["titleBold"] as? Bool, let subtitleBold = heading["subtitleBold"] as? Bool,
      let titleColor = headingColor("titleColor"), let subtitleColor = headingColor("subtitleColor"),
      let paperWhiteOverlay = configuration.paperWhiteOverlay ?? (heading["paperWhiteOverlay"] as? Double),
      configuration.columnLabels.count == 3, (0...1).contains(paperWhiteOverlay) else {
    FileHandle.standardError.write(Data("请检查标题样式、三列表头和背景白色叠加比例\n".utf8))
    exit(1)
}
let columnLabels = configuration.columnLabels

struct Row: Decodable {
    let name: String
    let on: Double, off: Double
    let minutes: Double?, on_off: Double?
    let avatar: String?, logo: String?

    var difference: Double { on_off ?? (on - off) }
}
let rows = sourceRows
    .sorted { $0.difference == $1.difference ? $0.name < $1.name : $0.difference > $1.difference }
precondition(!rows.isEmpty, "数据不能为空")
precondition(rows.allSatisfy { row in
    let minutes = row.minutes ?? 0
    return [minutes, row.on, row.off, row.difference].allSatisfy(\.isFinite)
        && minutes >= 0 && abs(row.on - row.off - row.difference) < 0.02
}, "请检查数值、差值及提供的分钟数")
try FileManager.default.createDirectory(at: outputDirectory, withIntermediateDirectories: true)
let width = 1452.0, height = 184.0 + Double(rows.count) * 54.0, scale = 2.0
let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: Int(width * scale), pixelsHigh: Int(height * scale), bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
let context = NSGraphicsContext(bitmapImageRep: bitmap)!
let cg = context.cgContext
cg.translateBy(x: 0, y: height * scale)
cg.scaleBy(x: scale, y: -scale)
NSGraphicsContext.current = NSGraphicsContext(cgContext: cg, flipped: true)
let ink = NSColor(srgbRed: 44/255, green: 62/255, blue: 80/255, alpha: 1)
let soft = NSColor(srgbRed: 86/255, green: 100/255, blue: 115/255, alpha: 1)
let lineColor = NSColor(srgbRed: 44/255, green: 62/255, blue: 80/255, alpha: 0.14)
let accent = NSColor(srgbRed: 217/255, green: 83/255, blue: 79/255, alpha: 1)
cg.clear(CGRect(x: 0, y: 0, width: width, height: height))

func typeface(_ size: Double, _ bold: Bool) -> NSFont {
    let primary = NSFont(name: bold ? "Menlo-Bold" : "Menlo-Regular", size: size)!
    let fallback = NSFontDescriptor(name: bold ? "PingFangSC-Semibold" : "PingFangSC-Regular", size: size)
    return NSFont(descriptor: primary.fontDescriptor.addingAttributes([.cascadeList: [fallback]]), size: size)!
}

func text(_ value: String, _ x: Double, _ y: Double, _ size: Double, _ color: NSColor, bold: Bool = false, align: String = "left") {
    if value.isEmpty { return }
    let font = typeface(size, bold)
    let value = NSAttributedString(string: value, attributes: [.font: font, .foregroundColor: color])
    let measured = value.size()
    let inkBounds = CTLineGetImageBounds(CTLineCreateWithAttributedString(value), nil)
    let left = align == "right" ? x - measured.width : align == "center" ? x - measured.width / 2 : x - inkBounds.minX
    value.draw(at: NSPoint(x: left, y: y))
}
func line(_ x1: Double, _ y1: Double, _ x2: Double, _ y2: Double, _ color: NSColor, thickness: Double = 1, dashed: Bool = false) {
    cg.saveGState(); cg.setStrokeColor(color.cgColor); cg.setLineWidth(thickness)
    cg.setLineCap(thickness > 1 ? .round : .butt)
    if dashed { cg.setLineDash(phase: 0, lengths: [4, 6]) }
    cg.move(to: CGPoint(x: x1, y: y1)); cg.addLine(to: CGPoint(x: x2, y: y2)); cg.strokePath(); cg.restoreGState()
}
func picture(_ path: String, _ rect: NSRect, alpha: Double = 1, circle: Bool = false) {
    let file = URL(fileURLWithPath: path, relativeTo: dataFile.deletingLastPathComponent())
    guard let image = NSImage(contentsOf: file) else { fatalError("无法读取素材：\(path)") }
    cg.saveGState()
    if circle { cg.addEllipse(in: rect); cg.clip() }
    let factor = circle ? max(rect.width / image.size.width, rect.height / image.size.height) : min(rect.width / image.size.width, rect.height / image.size.height)
    let target = NSRect(x: rect.midX - image.size.width * factor / 2, y: circle ? rect.minY : rect.midY - image.size.height * factor / 2, width: image.size.width * factor, height: image.size.height * factor)
    image.draw(in: target, from: .zero, operation: .sourceOver, fraction: alpha, respectFlipped: true, hints: [.interpolation: NSImageInterpolation.high])
    cg.restoreGState()
}
func marker(_ row: Row, _ x: Double, _ y: Double, _ color: NSColor, faded: Bool) {
    // Clear the connector under the marker before drawing its optional images.
    cg.saveGState(); cg.setBlendMode(.clear)
    cg.fillEllipse(in: CGRect(x: x-22, y: y-22, width: 44, height: 44)); cg.restoreGState()
    cg.saveGState(); cg.addEllipse(in: CGRect(x: x-20, y: y-20, width: 40, height: 40)); cg.clip()
    cg.setAlpha(faded ? 0.5 : 1)
    cg.beginTransparencyLayer(auxiliaryInfo: nil)
    if row.avatar != nil, let logo = row.logo {
        picture(logo, NSRect(x: x-34, y: y-34, width: 68, height: 68), alpha: 0.75)
    }
    if let icon = row.avatar ?? row.logo {
        picture(icon, NSRect(x: x-20, y: y-20, width: 40, height: 40), circle: row.avatar != nil)
    }
    cg.endTransparencyLayer()
    cg.restoreGState()
    cg.setStrokeColor((faded ? soft.withAlphaComponent(0.5) : color).cgColor); cg.setLineWidth(2.4)
    cg.strokeEllipse(in: CGRect(x: x-21, y: y-21, width: 42, height: 42))
}
func tone(_ index: Int) -> NSColor {
    let ratio = rows.count == 1 ? 0.5 : Double(index) / Double(rows.count - 1)
    let intensity = abs(ratio - 0.5) * 2
    let root = ratio < 0.5 ? NSColor(srgbRed: 0.168, green: 0.532, blue: 0.302, alpha: 1) : NSColor(srgbRed: 0.771, green: 0.204, blue: 0.169, alpha: 1)
    return soft.blended(withFraction: intensity, of: root)!
}
func number(_ value: Double, signed: Bool = false) -> String {
    let rounded = (value * 10).rounded() / 10
    return (signed && rounded > 0 ? "+" : "") + String(format: "%.1f", rounded == 0 ? 0 : rounded)
}
let values = rows.flatMap { [$0.on, $0.off] }
let low = values.min()!, high = values.max()!
let padding = high == low ? max(abs(low) * 0.025, 1) : (high-low) * 0.035
let minValue = low-padding, maxValue = high+padding
let plotLeft = 530.0, plotRight = width-110
func x(_ value: Double) -> Double { plotLeft + (value-minValue)/(maxValue-minValue)*(plotRight-plotLeft) }
let raw = (maxValue-minValue)/6, magnitude = pow(10, floor(log10(raw)))
let normalized = raw / magnitude
let step = (normalized <= 1 ? 1.0 : normalized <= 2 ? 2.0 : normalized <= 5 ? 5.0 : 10.0) * magnitude
var tick = ceil(minValue/step)*step
text(configuration.title, 40, 17, titleSize, titleColor, bold: titleBold)
text(configuration.subtitle ?? "", 40, 68, subtitleSize, subtitleColor, bold: subtitleBold)
text(configuration.playerLabel, 40, 129, 11, soft)
for (index, label) in columnLabels.enumerated() {
    let right = 310.0 + Double(index)*80
    text(label, right-13, 129, 11, index == 2 ? accent : soft, align: "right")
    let c = index == 2 ? accent : soft, center = right-5
    if index == 2 {
        line(center, 130, center, 142, c, thickness: 1.3)
        line(center, 142, center-3, 139, c, thickness: 1.3); line(center, 142, center+3, 139, c, thickness: 1.3)
    } else {
        line(center-2, 135, center-2, 130, c, thickness: 1.3)
        line(center-2, 130, center-4, 132, c, thickness: 1.3); line(center-2, 130, center, 132, c, thickness: 1.3)
        line(center+2, 137, center+2, 142, c, thickness: 1.3)
        line(center+2, 142, center, 140, c, thickness: 1.3); line(center+2, 142, center+4, 140, c, thickness: 1.3)
    }
}
while tick <= maxValue {
    text(String(format: "%g", tick), x(tick), 129, 11, soft, align: "center")
    line(x(tick), 160, x(tick), height-24, lineColor, dashed: true)
    tick += step
}
line(40, 159.5, width-40, 159.5, ink.withAlphaComponent(0.22))
for (index, row) in rows.enumerated() {
    let y = 187.0 + Double(index)*54
    let color = tone(index)
    line(40, y+26.5, width-40, y+26.5, lineColor)
    let icon = row.avatar ?? row.logo
    let nameX = icon == nil ? 40.0 : 82.0
    if let icon {
        picture(icon, NSRect(x: 40, y: y-17, width: 34, height: 34), circle: row.avatar != nil)
        cg.setStrokeColor(lineColor.cgColor); cg.setLineWidth(1); cg.strokeEllipse(in: CGRect(x: 40.5, y: y-16.5, width: 33, height: 33))
    }
    let nameFont = typeface(13, true)
    let measured = (row.name as NSString).size(withAttributes: [.font: nameFont]).width
    text(row.name, nameX, y - (row.minutes == nil ? 8 : 15), min(13, 13*(230-nameX)/max(1,measured)), ink, bold: true)
    if let minutes = row.minutes {
        text(String(format: "%.0f MIN", minutes.rounded()), nameX, y+4, 10, soft)
    }
    text(number(row.on), 310, y-8, 13, ink, align: "right")
    text(number(row.off), 390, y-8, 13, ink, align: "right")
    text(number(row.difference, signed: true), 470, y-11, 17, color, bold: true, align: "right")
    line(x(row.off), y, x(row.on), y, color, thickness: 3)
    marker(row, x(row.off), y, color, faded: true)
    marker(row, x(row.on), y, color, faded: false)
}
try bitmap.representation(using: .png, properties: [:])!.write(to: transparentFile)
print("已保存 \(transparentFile.path)（\(bitmap.pixelsWide)×\(bitmap.pixelsHigh)）")
let composed = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: Int(width * scale), pixelsHigh: Int(height * scale), bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
let paperContext = NSGraphicsContext(bitmapImageRep: composed)!.cgContext
paperContext.translateBy(x: 0, y: height * scale); paperContext.scaleBy(x: scale, y: -scale)
NSGraphicsContext.current = NSGraphicsContext(cgContext: paperContext, flipped: true)
let fullRect = NSRect(x: 0, y: 0, width: width, height: height)
// Tile without distorting the paper grain; the foreground remains a separate alpha layer.
guard let paper = NSImage(contentsOf: root.appendingPathComponent("素材/斑驳报纸.png")) else { fatalError("缺少背景：素材/斑驳报纸.png") }
for tileY in stride(from: 0.0, to: height, by: 740.0) {
    for tileX in stride(from: 0.0, to: width, by: 740.0) {
        paper.draw(in: NSRect(x: tileX, y: tileY, width: 740, height: 740), from: .zero, operation: .copy, fraction: 1, respectFlipped: true, hints: [.interpolation: NSImageInterpolation.high])
    }
}
let foreground = NSImage(size: NSSize(width: width * scale, height: height * scale))
NSColor.white.withAlphaComponent(paperWhiteOverlay).setFill()
paperContext.fill(fullRect)
foreground.addRepresentation(bitmap)
foreground.draw(in: fullRect, from: .zero, operation: .sourceOver, fraction: 1, respectFlipped: true, hints: nil)
try composed.representation(using: .png, properties: [:])!.write(to: paperFile)
print("已保存 \(paperFile.path)；字体：\(typeface(13, false).fontName)、\(typeface(13, true).fontName)")
