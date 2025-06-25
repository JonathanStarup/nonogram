import javax.swing.JFrame
import java.awt.event.WindowListener
import java.awt.event.WindowEvent
import java.awt.Canvas
import java.awt.Color
import java.awt.Font
import java.awt.Graphics

def main(): Unit \ IO + Chan = {
    let {frame = _frame, waitForExit} = makeFrame({
        title = "Demo",
        width = 800,
        height = 450,
        init = g -> {
            g.setColor(Color.red);
            // set Font
            g.setFont(new Font("Bold", 1, 20));
            // draw a string
            g.drawString("Demo", 800/8, 450/6)
        }
    });
    waitForExit()
}

type alias FrameConfig[ef: Eff] = {
    title = String,
    height = Int32,
    width = Int32,
    init = Graphics -> Unit \ ef
}

type alias Frame = {
    frame = JFrame,
    waitForExit = Unit -> Unit \ Chan
}

def makeFrame(config: FrameConfig[ef]): Frame \ IO + Chan + ef = {
    let {title, height, width, init} = config;
    let (send, recv) = Channel.buffered(1);
    def waitForClose() = {
        // Unit only has one value, so no non-determinism.
        unsafely NonDet run Channel.recv(recv);
        Channel.send((), send)
    };
    let f = new JFrame(title);
    f.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    f |> addOnExitListener(() -> Channel.send((), send));
    f.setSize(width, height);
    checked_ecast(f.add(new Canvas {
        def paint(_this: Canvas, g: Graphics): Unit \ ef = {
            init(g)
        }
    }));
    f.setResizable(false);
    f.setVisible(true);
    {frame = f, waitForExit = waitForClose}
}

def addOnExitListener(f: Unit -> Unit \ ef, frame: JFrame): Unit \ IO = {
    frame.addWindowListener(new WindowListener {
        def windowDeiconified(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowActivated(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowIconified(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowClosing(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowClosed(_this: WindowListener, _: WindowEvent): Unit \ ef = {
            f()
        }
        def windowDeactivated(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowOpened(_this: WindowListener, _: WindowEvent): Unit = ()
    })
}
