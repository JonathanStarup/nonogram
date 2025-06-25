import java.awt.{Graphics => JGraphics}
import java.awt.event.{MouseListener => JMouseListener}
import java.awt.event.{MouseMotionListener => JMouseMotionListener}
import java.awt.event.MouseEvent
import java.awt.event.WindowEvent
import java.awt.event.WindowListener
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.awt.image.ImageObserver
import javax.swing.JComponent
import javax.swing.JFrame

use Graphics.Color
use Graphics.runApp
use Graphics.PaintEvent

def main(): Unit \ IO = region rc {
    let paint = Ref.fresh(rc, Color.Black);
    runApp({
        title = "Demo",
        width = 800,
        height = 450,
        init = g -> {
            Graphics.fillRect({x = 0, y = 0}, {width = 800, height = 450}, Graphics.Color.Black, g)
        },
        mouseListener = {
            onPress = (event, g) -> {
                if (event.getButton() == MouseEvent.BUTTON1) Ref.put(Color.Red, paint)
                else Ref.put(Color.Black, paint);
                Graphics.fillCircle({x = event.getX(), y = event.getY()}, {radius = 20}, Ref.get(paint), g);
                PaintEvent.Repaint
            },
            onRelease = (_, _) -> PaintEvent.Nothing,
            onDrag = (event, g) -> {
                Graphics.fillCircle({x = event.getX(), y = event.getY()}, {radius = 20}, Ref.get(paint), g);
                PaintEvent.Repaint
            }
        }
    })
}
