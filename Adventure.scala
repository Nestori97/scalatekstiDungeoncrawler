package o1.adventure


/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the adventure game. */
  val title = "lost in the castle"

  private val start     = new Area("long hallway", "you are in a  long hallway filled with burning candles ")
  private val lavaroom= new Area("lava room ", "you are in a room filled with molten lava ")
  private val storage = new Area("storage", "you are in a small storage ")
  private  val stageroom=new Area("Stageroom","you stand in a room there is an acting stage and weird action figutines made of paper ")
  private val  nextTolastBossRoom  = new Area("small corridor", "you are at a smaal corridor before you stand a strong door\n you feel a strong precense behind that door ")
  private val lastbossroom       = new Area("altar", "you see your princess behind that menacing monster ")
  private val destination = lastbossroom
  storage.makeAShop()
  nextTolastBossRoom.makeAShop()
  private var luckyNumber=0//käytetään pohjana  useamman arvon pohjana sekä vahingon laskennassa

  start.setNeighbors(Vector("north" -> lavaroom, "east" -> storage          ))
  lavaroom.setNeighbors(Vector( "north"  -> nextTolastBossRoom ,                "south" -> start,      "west" -> storage  ))
  nextTolastBossRoom.setNeighbors(Vector("north" -> lastbossroom,       "south" -> lavaroom, "west" -> stageroom   ))
  storage.setNeighbors(Vector(                                   "east" -> lavaroom,                        "west" -> start))
  stageroom.setNeighbors(Vector("east" -> nextTolastBossRoom))
  lastbossroom.setNeighbors(Vector(                                              "south" ->nextTolastBossRoom ))


  this.start.addItem(new Item("watergun","hey atleast its better than nothing",5,10,"damage"))
  this.stageroom.addItem(new Item("papersword","made of paper but suprisingly strong",20,10,"damage"))
  this.lastbossroom.addItem(new Item("spike","seems like its has dropped from bowsers back",1,1,"damage"))

  /** The character that the player controls in the game. */
  val player = new Player(start,20,luckyNumber)
  val lastBoss=new Bowser(lastbossroom,100,20,"Bowser")//needs to be defeated before you can win the game
  this.lavaroom.addEnemy(new Blooper(lavaroom,30,12,"ghost squid"))
  this.start.addEnemy(new Toad(start,15,20,"small toad"))
  this.stageroom.addEnemy( new Goomba(stageroom,40,2,"walking brown mushroomhead"))
  nextTolastBossRoom.addEnemy(new Goomba(nextTolastBossRoom,25,3,"walking brown mushroomhead"))

  this.lastbossroom.addEnemy(lastBoss)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  var timeLimit = 40


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = {
   this.player.location == this.destination&&lastBoss.isDead
  }

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit||this.player.healthPoints<=0

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "You stand alone in a room thats filled with pictures of weird creature thas partly a turtle and partly a dinosaur with spikes on its shell \nyou hear a womans's faint whisper from a distance\nIt sounds familiar and it seems to be calling you\nyou head towards the sound. fighting your way thourg hordes of enemies"
  def getARandomNumber(StringContainingTheNumber:Some[String])={///lukee aluksi annetun onnenluvun mutta muuttaa sen ensinmäisen merkin acsii taulukkoa vastaavaksi pitää myös huolta siitä että myös muut kuin numerot hyväksytään
    ///lisäksi määrää pelin pituuden annetun merkin mukaan.Hiljaisuus tuomittakoon.
      if (StringContainingTheNumber.isDefined&&StringContainingTheNumber.get!=""){
      val arvo=StringContainingTheNumber.get.charAt(0)
        luckyNumber=arvo.toInt
        this.player.luckyNumber=luckyNumber
        this.player.setHealth
         timeLimit=luckyNumber
        "I see,in this world your lucky number translates to "+luckyNumber
      }
      else {
        //tyhjän syötteen poikkeukselle oma kohta
         timeLimit=luckyNumber
          "If I were you I would have said something"

      }
  }

  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "finally you reach to the source of the whispers,you see a a mix between a human and mushroom.He welcomes you with a message:Thank you but our princess is in another castle !"
    else if (this.player.healthPoints<=0) "you feel dizzy,it seems like your strenght was not enough to prevail this time"
    else if (this.turnCount == this.timeLimit)
        "your time run out ,you can't hear the woman anymore\n it seems like your adventure has come to an end"
    else  // game over due to player quitting
      "Quitter!"
  }


  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined) {
      this.turnCount += 1
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }


}

