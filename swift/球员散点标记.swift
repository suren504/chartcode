import AppKit

// 圆形裁剪和渐隐描边沿用 nba_site 的 PlayerScatterAvatar。
// 将生成的 -on.png 或 -off.png 填入散点图相应数据点的 logo 字段。
let args = CommandLine.arguments
guard args.count == 5 else {
    print("用法：swift swift/球员散点标记.swift 头像路径 队标路径 '#RRGGBB' 输出前缀")
    exit(args.count == 1 ? 0 : 1)
}

let color = args[3]
guard color.count == 7, color.hasPrefix("#"), let rgb = UInt32(color.dropFirst(), radix: 16) else {
    FileHandle.standardError.write(Data("球队主色须为 #RRGGBB\n".utf8))
    exit(1)
}

let teamColor = NSColor(srgbRed: Double((rgb >> 16) & 255) / 255,
                        green: Double((rgb >> 8) & 255) / 255,
                        blue: Double(rgb & 255) / 255, alpha: 1)
let paper = NSColor(srgbRed: 1, green: 250.0 / 255, blue: 240.0 / 255, alpha: 1)
let size = 200
let frame = CGRect(x: 0, y: 0, width: size, height: size)
let avatarFrame = CGRect(x: 28, y: 28, width: 176, height: 176)
let borderWidth = CGFloat(size) * 2 / 62
let borderPath = CGPath(ellipseIn: frame.insetBy(dx: borderWidth / 2, dy: borderWidth / 2),
                        transform: nil)
    .copy(strokingWithWidth: borderWidth, lineCap: .butt, lineJoin: .miter, miterLimit: 10)
// 从顶部算起，35% 处开始显色，90% 处达到球队主色的全不透明度。
let borderColors = [0.0, 0, 1, 1].map { teamColor.withAlphaComponent($0).cgColor }
let borderGradient = CGGradient(colorsSpace: CGColorSpace(name: CGColorSpace.sRGB),
                                colors: borderColors as CFArray, locations: [0, 0.35, 0.90, 1])!

func loadImage(_ path: String) throws -> NSImage {
    guard let image = NSImage(contentsOf: URL(fileURLWithPath: path)),
          image.isValid, image.size.width > 0, image.size.height > 0 else {
        throw CocoaError(.fileReadCorruptFile, userInfo: [NSFilePathErrorKey: path])
    }
    return image
}

func draw(_ image: NSImage, in rect: CGRect, opacity: CGFloat = 1) {
    image.draw(in: rect, from: .zero, operation: .sourceOver, fraction: opacity,
               respectFlipped: true, hints: [.interpolation: NSImageInterpolation.high])
}

do {
    let avatar = try loadImage(args[1])
    let logo = try loadImage(args[2])
    let avatarScale = max(avatarFrame.width / avatar.size.width, avatarFrame.height / avatar.size.height)
    let avatarWidth = avatar.size.width * avatarScale
    let avatarRect = CGRect(x: avatarFrame.midX - avatarWidth / 2, y: avatarFrame.minY,
                            width: avatarWidth, height: avatar.size.height * avatarScale)
    let prefix = URL(fileURLWithPath: args[4])
    let directory = prefix.deletingLastPathComponent()
    try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)

    for (state, opacity) in [("on", CGFloat(1)), ("off", CGFloat(0.5))] {
        let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: size, pixelsHigh: size,
                                     bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
                                     colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
        let context = NSGraphicsContext(bitmapImageRep: bitmap)!.cgContext
        context.clear(frame)
        context.translateBy(x: 0, y: CGFloat(size))
        context.scaleBy(x: 1, y: -1)
        NSGraphicsContext.current = NSGraphicsContext(cgContext: context, flipped: true)

        context.saveGState()
        context.addEllipse(in: frame)
        context.clip()
        draw(logo, in: frame.insetBy(dx: -10, dy: -10))
        context.clip(to: avatarFrame)
        if state == "off" {
            // 用头像的 Alpha 轮廓限制底色，再把 50% 不透明度的真实头像覆在底色上。
            context.beginTransparencyLayer(auxiliaryInfo: nil)
            draw(avatar, in: avatarRect)
            context.setBlendMode(.sourceIn)
            context.setFillColor(paper.cgColor)
            context.fill(avatarFrame)
            context.endTransparencyLayer()
        }
        draw(avatar, in: avatarRect, opacity: opacity)
        context.restoreGState()

        context.addPath(borderPath)
        context.clip()
        context.drawLinearGradient(borderGradient, start: .zero, end: CGPoint(x: 0, y: size), options: [])
        NSGraphicsContext.current = nil

        let output = directory.appendingPathComponent("\(prefix.lastPathComponent)-\(state).png")
        guard let png = bitmap.representation(using: .png, properties: [:]) else {
            throw CocoaError(.fileWriteUnknown, userInfo: [NSFilePathErrorKey: output.path])
        }
        try png.write(to: output, options: .atomic)
        print("已保存 \(output.path)")
    }
} catch {
    FileHandle.standardError.write(Data("生成标记失败：\(error.localizedDescription)\n".utf8))
    exit(1)
}
