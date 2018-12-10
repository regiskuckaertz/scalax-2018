import scalaz.zio.{IO, Ref}
import java.io.IOException
import scalaz._, Scalaz._
import data._
import tc._

object VDOM {

  // currently missing in scalaz
  implicit class RichComonad[W[_], A](val value: W[A])(implicit C: Comonad[W]) {
    def copoint: A = C.copoint(value)
    def cojoin: W[W[A]] = C.cojoin(value)
  }

  implicit class RichString(val str: String) extends AnyVal {
    def wrap(len: Int, padding: Int): List[String] = {
      val spaces = " ".times(padding)
      val lines = str.grouped(len - padding * 2).map { str =>
        val padRight = " ".times(len - padding * 2 - str.length)
        spaces ++ str ++ padRight ++ spaces
      }.toList
      val emptylines = List.fill(15 - lines.length)(" ".times(len))
      lines ++ emptylines
    }

    def times(n: Int): String = List.fill(n)(str).mkString

    def white: String = s"${Console.WHITE}${str}${Console.RESET}"

    def bold: String = s"${Console.BOLD}${str}${Console.RESET}"

    def invert: String = s"${Console.REVERSED}${str}${Console.RESET}"

    def blink: String = s"${Console.BLINK}${str}${Console.RESET}"
  }

  type UI[Base[_], M[_], A] = (M[Unit] => Base[Unit]) => A

  type Component[Base[_], W[_], M[_], A] = W[UI[Base, M, A]]

  case class Tab(label: String, content: String, keystroke: String => IO[IOException, Unit])

  type TabbedUI = Component[IO[IOException, ?], Zipper, ZipMove, Tab]

  def tabbedUI(t: (String, String), ts: (String, String)*): TabbedUI = {
    def render(t: (String, String)): UI[IO[IOException, ?], ZipMove, Tab] = send =>
      Tab(
        label     = t._1,
        content   = t._2,
        keystroke = {
          case "p" => send(ZipLeft(ZipStop(())))
          case "n" => send(ZipRight(ZipStop(())))
          case _   => IO.unit
        }
      )

    Zipper.zipper(
      Stream.empty,
      render(t),
      Stream(ts:_*).map(render)
    )
  }

  def move[M[_], W[_]: Comonad, A](action: M[Unit], component: W[A])(implicit P: Pairing[M, W]): W[A] =
    P.pair(action, component.cojoin)((_, wa) => wa)

  def buildNav(tabs: Zipper[Tab]): (String, Int) = {
    val lengths = tabs.map { t => (t == tabs.focus, t.label.length) }
    val top = " " + lengths.map { case (f, l) => if (f) "┌─" + List.fill(l)("─").mkString + "─┐" else List.fill(l + 4)(" ").mkString }.toStream.mkString + " "
    val nav = " " + tabs.map { t => if (t == tabs.focus) "│ " + t.label.bold.white + " │" else " " + s" ${t.label} ".invert + " " }.toStream.mkString + " "
    val bot = "┌" + lengths.map { case (f, l) => if (f) "┘ " + List.fill(l)(" ").mkString + " └" else List.fill(l + 4)("─").mkString }.toStream.mkString + "┐"

    (top + "\n" + nav + "\n" + bot, top.length)
  }

  def mkString(s: String, m: String, e: String)(xs: List[String]) = xs match {
    case Nil => ""
    case _ => xs.mkString(s, m, e)
  }

  def send(state: Ref[TabbedUI], current: TabbedUI): ZipMove[Unit] => IO[IOException, Unit] = 
    action => state.set(move[ZipMove, Zipper, UI[IO[IOException, ?], ZipMove, Tab]](action, current))
        
  def run(component: TabbedUI): IO[IOException, Unit] = {
    for {
      state <- Ref(component)
      _ <- (state.get.flatMap { current =>

        val (nav, width) = buildNav(current.map(_(send(state, current))))

        val tab: Tab = Comonad[Zipper].copoint(current)(send(state, current)(_))

        val buttons: String = "[n]ext, [p]revious or [q]uit?"

        zio.console.putStrLn(nav) *>
          zio.console.putStrLn(s"│" + " ".times(width - 2) + "│") *>
          zio.console.putStrLn(tab.content.wrap(width - 2, 2).map("│" + _ + "│").mkString("\n")) *>
          zio.console.putStrLn("├" + "─".times(width - 2) + "┤") *>
          zio.console.putStrLn("│  " + "[" + "n".bold + "]ext, [" + "p".bold + "]revious or [" + "q".bold + "]uit?" + " ".times(width - buttons.length - 4) + "│") *>
          zio.console.putStrLn("└" + "─".times(width - 2) + "┘") *>
          zio.console.getStrLn.flatMap {
            case "q" => IO.terminate
            case x => tab.keystroke(x)
          }
      }).forever
    } yield ()
  }

  val example = tabbedUI(
    "The 🐰 hole" -> "Alice was beginning to get very tired of sitting by her sister on the bank, and of having nothing to do: once or twice she had peeped into the book her sister was reading, but it had no pictures or conversations in it, ‘and what is the use of a book,’ thought Alice ‘without pictures or conversations?’",

    "Pool of 😭" -> "‘Curiouser and curiouser!’ cried Alice (she was so much surprised, that for the moment she quite forgot how to speak good English); ‘now I’m opening out like the largest telescope that ever was! Good-bye, feet!’ (for when she looked down at her feet, they seemed to be almost out of sight, they were getting so far off). ‘Oh, my poor little feet, I wonder who will put on your shoes and stockings for you now, dears? I’m sure I shan’t be able! I shall be a great deal too far off to trouble myself about you: you must manage the best way you can;—but I must be kind to them,’ thought Alice, ‘or perhaps they won’t walk the way I want to go! Let me see: I’ll give them a new pair of boots every Christmas.’",

    "A Caucus-🎽" -> "They were indeed a queer-looking party that assembled on the bank—the birds with draggled feathers, the animals with their fur clinging close to them, and all dripping wet, cross, and uncomfortable. The first question of course was, how to get dry again: they had a consultation about this, and after a few minutes it seemed quite natural to Alice to find herself talking familiarly with them, as if she had known them all her life. Indeed, she had quite a long argument with the Lory, who at last turned sulky, and would only say, ‘I am older than you, and must know better’; and this Alice would not allow without knowing how old it was, and, as the Lory positively refused to tell its age, there was no more to be said.",

    "The 💸" -> "It was the White Rabbit, trotting slowly back again, and looking anxiously about as it went, as if it had lost something; and she heard it muttering to itself ‘The Duchess! The Duchess! Oh my dear paws! Oh my fur and whiskers! She’ll get me executed, as sure as ferrets are ferrets! Where can I have dropped them, I wonder?’ Alice guessed in a moment that it was looking for the fan and the pair of white kid gloves, and she very good-naturedly began hunting about for them, but they were nowhere to be seen—everything seemed to have changed since her swim in the pool, and the great hall, with the glass table and the little door, had vanished completely.",

    "Advice from 🐛" -> "The Caterpillar and Alice looked at each other for some time in silence: at last the Caterpillar took the hookah out of its mouth, and addressed her in a languid, sleepy voice. ‘Who are you?’ said the Caterpillar. This was not an encouraging opening for a conversation. Alice replied, rather shyly, ‘I—I hardly know, sir, just at present—at least I know who I was when I got up this morning, but I think I must have been changed several times since then.’ ‘What do you mean by that?’ said the Caterpillar sternly. ‘Explain yourself!’ ‘I can’t explain myself, I’m afraid, sir’ said Alice, ‘because I’m not myself, you see.’"
  )
}