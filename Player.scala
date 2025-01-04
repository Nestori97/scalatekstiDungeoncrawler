package o1.adventure

import scala.collection.mutable.Map
import scala.util.Random


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area,var healthPoints:Double,var luckyNumber:Double) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var tavarat=Map[String,Item]()
  private val hero="Hero"
  private var gold=0
  private var maxHealthPoints=healthPoints*3
  private var luck=new Random(luckyNumber.toInt)
  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven
  def setHealth={///vähän typerä toteutus mutta halusin healthpointsien sekä random generaattorin riippun annetusta arvosta siksi ne ovat var sillä uusi hahmo luodaa aivan aluksi ennen kuin lukuja kysytään joten niille on täytynyt antaa jotkin alkuarvot
      healthPoints=luckyNumber*3
      maxHealthPoints=healthPoints
      luck=new Random(luckyNumber.toInt)
  }

  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) = {
    if(this.currentLocation.hasEnemy)"the"+this.location.getEnemy.values.head.name+" blocks the way\n" +
      "you must defeat it first before you can continue on your search of the mysterius voice"
    else if (this.tavarat.isEmpty)"its dangerous out there, you better pick up weapon before you continue"
    else{
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if (destination.isDefined) "You go " + direction + "." else "You can't go " + direction + "."
    }
  }

  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though."
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }
  def drop(itemName: String): String={
      if(tavarat.contains(itemName)){
         this.currentLocation.addItem(this.tavarat(itemName))
         tavarat-=itemName
         "You drop the "+itemName+"."
      }
      else "You don't have that!"
  }
  def examine(itemName: String): String={
      if(tavarat.contains(itemName)){
        "You look closely at the "+itemName+"\n"+tavarat(itemName).description
      }
      else  "If you want to examine something, you need to pick it up first."
  }
  def get(itemName: String) = {
    val received = this.location.removeItem(itemName)
    for (newItem <- received) {
      this.tavarat.put(newItem.name, newItem)
    }
    if (received.isDefined) "You pick up the " + itemName + "." else "There is no " + itemName + " here to pick up."
  }
  def has(itemName: String): Boolean={
         this.tavarat.contains(itemName)
  }
  def inventory: String={
      if(this.tavarat.isEmpty) "You are empty-handed."
      else{
          "You are carrying:"+"\n" + this.tavarat.keys.mkString("\n")
      }
  }
  def attack(weapon:String)={///pitää huolen vaurioiden kirjaamisesta sekä vihollisen kuolemisesta mutta pelaajan elämänpisteiden riittävyydestä tämä ei ota kantaa
         def calculateDamage(attacker:String,defender:String):Double={///käytetään vaurioiden laskemiseen hyödyntäen alussa annettua onnenlukua
            val enemy=this.currentLocation.getEnemy.values.head
            if(attacker==this.hero){
            val dealtDamage=enemy.ability(weapon)*tavarat(weapon).damage
            enemy.healthPoints-=dealtDamage
            dealtDamage
            }
            else {
            this.healthPoints-=enemy.damage*((127.0-luckyNumber)/100)
            enemy.damage*((127.0-luckyNumber)/100)
            }
        }
      if(this.tavarat.contains(weapon)){
         if(this.currentLocation.getEnemy.nonEmpty) {
            val enemy=this.currentLocation.getEnemy.values.head///otetaan ensinmäisenä mapissa esiintyvä vihollinen
           val damage=calculateDamage(this.hero,enemy.name)
            if(enemy.isDead){
              val enemyGold=enemy.getGold
              this.gold+=enemyGold
              this.location.removeEnemy(enemy.name)//poistaa vihollisen alueelta
               "you dealt "+damage+" points of damage and it was a killing blow. The monster dropped "+enemyGold+" gold "
             }
            else{
              "you dealt "+damage+" points of damage"+" but the "+enemy.name+" retaliated dealing "+ calculateDamage(enemy.name,this.hero)+
              " points of damage to you leaving you with "+this.healthPoints+" healthpoints"
            }
          }
         else{
           "there are no enemies nearby"
         }
      }
      else "you have no such weapon named "+weapon
  }
  def buy(itemname:String)={///ostaa kaupasta tavaran ja poistaa sen kaupan valikoimasta.tämä oli siis tarkoitus mutta en saanut kauppaa toimimaan järkevästi
    // jossa se tulostaisi aina jäljellä olevien tuotteiden hinnat ja määrät joten päädyin ratkaisuun jossa voit ostaa vain yhden tuotteen. jälkiä yrityksestä voi löytyä vielä koodista vaikka yritin niitä poistaa
      if(this.currentLocation.hasAShop){
        val shop=this.currentLocation.getAShop
         if(shop.itemlist.contains(itemname)){
           val hinta=shop.itemlist(itemname).price
            if(shop.itemlist(itemname).price<=gold){
              val originalgold=gold
              gold=gold-shop.itemlist(itemname).price
              val item=shop.removeItem(itemname).get
              tavarat+=item.name->item
              val goldAfterTrade=originalgold-gold
              this.currentLocation.removeShop
              "you bought "+itemname+" for "+goldAfterTrade+" gold leaving you with "+gold
            }
            else "you don't have enough gold"
         }
         else "there is none such item for sale here"
      }
      else "there seems to be no shop here"
  }
  def use(itemname:String)={///hyvin simppeli toteutus jossa tavarat on jaettu kahteen tyyppiin joko parantaviin tai vaurioittaviin
    def whatTheItemDid(itemType:String)={
        val item=tavarat(itemname)
        if (itemType=="healing"){
            val healing=item.damage*luckyNumber
            tavarat-=itemname
            if(this.healthPoints+healing>=this.maxHealthPoints){
               this.healthPoints=this.maxHealthPoints
               "you drank the most potent healing elixir restoring your health to full"
            }
            else "you drank the whole potion restoring "+healing+" healthpoints but you are still feeling weak"
        }
       else {
            if(luck.nextInt(10)>6){
              healthPoints+=2
              maxHealthPoints+=2
              "you took "+itemname+" to your hands and felt a surge determination going through your veins your max health increased by 2 to "+this.maxHealthPoints
            }
            else
              {
              healthPoints-=4
              maxHealthPoints-=4
                "you took "+itemname+" to your hands and you felt an overwhelming feeling of dread. your max health has reduced by 4 to " + this.maxHealthPoints
              }
        }
    }
    if(this.tavarat.contains(itemname)){
       whatTheItemDid(tavarat(itemname).itemType)
      }

    else "you carrying no such item with you"
  }
  def help()={
      "the commands from older versions still work(but go command has conditions when its useable now.The conditions are:Area must be clear of enemies before allowed to leave and you must have something in your inventory).\nNew command are attack,buy,use\n" +
        "to use commands attack you must have a weapon in your inventory. using the command goes like this attack itemname\n"+
        "buy command allows you to buy stuff if there is shop in area and you have enough gold.using the command goes like this buy itemname\n"+
        "use command takes item and either heals you or increases or decreases your max health depending on your items type and your luck.using the command goe like this use itemname\n "+
    "if you are stuck in first room use command get watergun and after that use command attack watergun to get the game going"


  }

  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name


}


