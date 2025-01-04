package o1.adventure

abstract class Enemy(startingArea: Area,var healthPoints:Int,val damage:Int,val name:String) {
      def isDead= !(this.healthPoints>0)
      def ability(itemName:String):Int
      def getGold:Int
//eri vihollisilla eri heikkouksia riippuen aseesta
}
class Toad(startingArea: Area, healthPoints:Int, damage:Int, name:String) extends Enemy(startingArea,healthPoints,damage,name){
     var damageMultiplier=1
     val gold=5
     def ability(itemName:String):Int={
         if(itemName=="watergun") damageMultiplier=2
         damageMultiplier
      }
     def getGold:Int={///saadun kullan määrä vaihtelee riippuen siitä kuinka hyvin pelaat
      gold*damageMultiplier
     }
}
class Goomba(startingArea: Area, healthPoints:Int, damage:Int, name:String) extends Enemy(startingArea,healthPoints,damage,name){
      var damageMultiplier=1
      val gold=20
      def ability(itemName:String):Int={
          if(itemName=="jumpingshoes") damageMultiplier=100
          damageMultiplier
      }
     def getGold:Int={
         gold*damageMultiplier/10
     }
}
class Blooper(startingArea: Area, healthPoints:Int, damage:Int, name:String) extends Enemy(startingArea,healthPoints,damage,name){
      var damageMultiplier =1.0
      val gold= 2
      def ability(itemName:String):Int= {
          if (itemName=="squidsquisher") damageMultiplier=4.2
          damageMultiplier.toInt
      }
     def getGold:Int={
       val goldAsDouble=gold*damageMultiplier*4
       goldAsDouble.toInt
     }
}
class Bowser(startingArea: Area, healthPoints:Int, damage:Int, name:String) extends Enemy(startingArea,healthPoints,damage,name){
      var damageMultiplier =0
      val gold =12
      var lastWeaponUsed=""
      var goldMultiplier=0
      def ability(itemName:String):Int= {
          if(itemName!=lastWeaponUsed) {
            goldMultiplier+=1
            damageMultiplier+=1
          }
          else damageMultiplier=0
          lastWeaponUsed=itemName
          damageMultiplier
      }
        def getGold:Int={
         gold*goldMultiplier
     }

}
