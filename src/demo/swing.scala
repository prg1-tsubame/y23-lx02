import scala.swing._

object SwingDemo {
    def main(): Unit = {
        new Frame {
            title = "Hello world"
            
            contents = new FlowPanel {
                contents += new Label("Launch rainbows:")
                contents += new Button("Click me") {
                    reactions += {
                        case event.ButtonClicked(_) =>
                        println("All the colours!")
                    }
                }
            }
    
            pack()
            centerOnScreen()
            open()
        }
    }
}

@main def runSwingDemo() = SwingDemo.main()