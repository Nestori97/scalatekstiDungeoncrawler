package o1.adventure

import scala.collection.mutable
import scala.collection.mutable.Map

/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. In general, an "area" can be pretty much anything: a room, a building,
  * an acre of forest, or something completely different. What different areas have in
  * common is that players can be located in them and that they can have exits leading to
  * other, neighboring areas. An area also has a name and a description.
  * @param name         the name of the area
  * @param description  a basic description of the area (typically not including information about items) */
class Area(var name: String, var description: String) {

  private val neighbors = Map[String, Area]()
  private var tavarat=Map[String,Item]()
  private var enemies=Map[String,Enemy]()
  private var kauppa=false
  private var theShop: Option[Shop]=None

  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)


  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }


  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction--area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }


  /** Returns a multi-line description of the area as a player sees it. This includes a basic
    * description of the area as well as information about exits and items. The return
    * value has the form "DESCRIPTION\n\nExits available: DIRECTIONS SEPARATED BY SPACES".
    * The directions are listed in an arbitrary order. */
  def fullDescription = {
    var startOfTheDescription=""
    val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(" ")

    if(this.tavarat.isEmpty){
       startOfTheDescription=this.description
    }
    else{
        val itemlist="You see here: "+this.tavarat.keys.mkString(" ")
         startOfTheDescription=this.description +"\n"+ itemlist
    }
    if(hasEnemy) startOfTheDescription+"\nyou see a menacing monster "+enemies.values.head.name+"\nit blocks all the paths and does not seem friendly"
    else{
      startOfTheDescription+"phew it seems like a moment of respite has been given to you as you see no monsters here but you hear the womans whiper again 'Hurry my dear'"+exitList+ this.getAShopDescription
  }
  }
  def addItem(item: Item): Unit={
      this.tavarat.put(item.name,item)
  }
  def contains(itemName: String): Boolean={
      tavarat.contains(itemName)
  }

  def removeItem(itemName: String): Option[Item]={
    tavarat.remove(itemName)
  }
  def addEnemy(enemy: Enemy)={
    this.enemies+=enemy.name->enemy
  }
  def getEnemy={
      this.enemies
  }
  def removeEnemy(enemyName:String): Unit ={
      this.enemies.remove(enemyName)
  }
  def hasEnemy= !enemies.isEmpty
  def hasAShop= kauppa
  def makeAShop()={
    kauppa=true
    theShop=Some( new Shop(this))
    this.theShop.get.addItem(new Item("potion","a potion of most potent healing elixir",1,8,"healing"))
    this.theShop.get.addItem(new Item("squidsquisher","seems to be made to squish squid",5,10,"damage"))
    this.theShop.get.addItem(new Item("jumpingshoes","shoes made for jumping on top of mushrooms",1,10,"damage"))
  }
  def getAShop=this.theShop.get
  def getAShopDescription={
      if( this.hasAShop){
        " on this room  you see a shop,you maybe able to buy something\n old toad human creature greets you 'I have one of each in stock:Potion,jumpingShoes,SquidSquisher' but beware this shop is unstable\n"+
          "you can only buy one item and after that I shall be going"
      }
      else ""
  }
  def removeShop={
      this.theShop=None/// pelätty None,on tässä vain sitä varten että olio shop katoaa muistista eikä jää ikuiseksi ajaksi syömään sitä. Ongelmia ei pitäisi ilmetä sillä boolena arvoa käytetää lippuna aina ennen kuin kauppaan yritetään päästä käsiksi
      kauppa=false
  }

  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)



}
class Shop(area: Area){///area annetaan parametriksi koska aluksi ajattelin hyödyntää itse aluetta mutta päädyin helpompaan ratkaisuun
    val itemlist=Map[String,Item]()
    def addItem(item: Item): Unit={
      this.itemlist.put(item.name,item)
    }
    def removeItem(itemName: String): Option[Item]={
       itemlist.remove(itemName)
    }


}
