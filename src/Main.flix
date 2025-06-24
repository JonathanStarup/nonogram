import javax.swing.JFrame
import java.awt.event.WindowListener
import java.awt.event.WindowEvent

def main(): Unit \ IO + Chan = {
    let (_frame, wait) = makeFrame("Demo", {w = 1600, h = 900});
    wait()
}

def makeFrame(title: String, dim: {h = Int32, w = Int32}): (JFrame, Unit -> Unit \ Chan) \ IO + Chan = {
    let (send, recv) = Channel.buffered(1);
    def waitForClose() = {
        // Unit only has one value, so no non-determinism.
        unsafely NonDet run Channel.recv(recv);
        Channel.send((), send)
    };
    let f = new JFrame(title);
    f.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    f |> addOnExitListener(() -> Channel.send((), send));
    f.setSize(dim#w, dim#h);
    f.setResizable(false);
    f.setVisible(true);
    (f, waitForClose)
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
