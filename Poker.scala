object Poker {
  
  def main(args : Array[String]) : Unit = {
    println(comparaMaos(Array("7H", "8H", "9H", "10H", "JH"), Array("10H", "JH", "QH", "KH", "AH")))    
  }
      
  def converteParaNumeros(cartas: List[String]): List[String] = {
    if (cartas.length == 0)
      List()
    else {
	      if (cartas.head.contains("A")) List(cartas.head.replaceFirst("A", "14")) ::: converteParaNumeros(cartas.tail)
	      else if (cartas.head.contains("K")) List(cartas.head.replaceFirst("K", "13")) ::: converteParaNumeros(cartas.tail)
	      else if (cartas.head.contains("Q")) List(cartas.head.replaceFirst("Q", "12")) ::: converteParaNumeros(cartas.tail)
	      else if (cartas.head.contains("J")) List(cartas.head.replaceFirst("J", "11")) ::: converteParaNumeros(cartas.tail)
	      else List(cartas.head) ::: converteParaNumeros(cartas.tail)
	    }
  }	
  
  /** Compara as mão, retorna 1 se a primeira mão ganhar e 0 se a segunda mão ganhar */
  def comparaMaos(cartasJog1: Array[String], cartasJog2: Array[String]): Int = {
    println("maior joagada do 1: " + maiorJogada(ordena(converteParaNumeros(cartasJog1.toList))))
    println("maior joagada do 2: " + maiorJogada(ordena(converteParaNumeros(cartasJog2.toList))))
	 if(maiorJogada(ordena(converteParaNumeros(cartasJog1.toList))) > 
	     maiorJogada(ordena(converteParaNumeros(cartasJog2.toList)))) 1
	 else 0
  }
  
  def maiorJogada(cartas: List[String]): Int = {
    if(JogadaSequenciaReal(cartas)) 10
    else if(JogadaSequenciaMesmoNaipe(cartas)) 9
    else if(JogadaQuadra(cartas)) 8
    else if(JogadaFullHouse(cartas)) 7
    else if(JogadaFlush(cartas)) 6
    else if(JogadaSequencia(cartas)) 5
    else if(JogadaTrinca(cartas)) 4
    else if(JogadaDoisPares(cartas)) 3
    else if(JogadaPar(cartas)) 2    
    else 1    
  }
  
  //-------------------------------------------------- Jogadas ------------------------------------------------------------------------------//
	
	/** Royal Straight Flush */
	def JogadaSequenciaReal(cartas: List[String]): Boolean = {
	  JogadaSequenciaMesmoNaipe(cartas) && cartas.last.substring(0, 2).equals("14")
	}
	
	/** Straight Flush */
	def JogadaSequenciaMesmoNaipe(cartas: List[String]): Boolean = {
	  JogadaSequencia(cartas) && JogadaFlush(cartas)
	}
	
	/** Four of a kind */
	def JogadaQuadra(cartas: List[String]): Boolean = {
	  if(cartas.length == 4) (cartas.count(c => getValor(c) == getValor(cartas.head)) == 4)
	  else (cartas.count(c => getValor(c) == getValor(cartas.head)) == 4) ||  JogadaQuadra(cartas.tail)	    
	}
	
	/** Flush - Testa se todos os naipes são iguais */
	def JogadaFlush(cartas: List[String]): Boolean = {
	  cartas.forall(c => c.endsWith(String.valueOf(cartas(0).charAt(cartas(0).length - 1))))
	}
	
	/** Full House or Full Hand */
	def JogadaFullHouse(cartas: List[String]): Boolean = {	  
    /*Remove valores iguais aos da cabeça. Quando pede para remover os valores 
     * iguais aos da cabeça do restante, se não sobrar nenhum na lista e ele 
     * não for uma quadra, é porque ele é um Full House
     */
	  val restante:List[String] = cartas.remove(s => getValor(s) == getValor(cartas.head))
	  (!JogadaQuadra(cartas)) && restante.remove(s => getValor(s) == getValor(restante.head)).isEmpty
	}
	
	/** Straight - Testa se as cartas estão em sequência */
	def JogadaSequencia(cartas: List[String]): Boolean = {
	  if (cartas.length == 2) { 
	    getValor(cartas.head) + 1 == getValor(cartas.last)
	  } else {
	     (getValor(cartas.head) + 1 == getValor(cartas.tail.head)) && JogadaSequencia(cartas.tail)
	  }
	}
	
	/** Three of a kind */
	def JogadaTrinca(cartas: List[String]): Boolean = {
	  if(cartas.length == 3) (cartas.count(c => getValor(c) == getValor(cartas.head)) == 3)
	  else (cartas.count(c => getValor(c) == getValor(cartas.head)) == 3) ||  JogadaTrinca(cartas.tail)
	}
		
	/** Two Pairs */
	def JogadaDoisPares(cartas: List[String]): Boolean = {
	  val restante:List[String] = cartas.remove(s => getValor(s) == getValor(cartas.head))
	  val restante2:List[String] = restante.remove(s => getValor(s) == getValor(restante.head))
	  (!JogadaTrinca(cartas)) && restante2.remove(s => getValor(s) == getValor(restante2.head)).isEmpty
	}
	
	/** One Pair */
	def JogadaPar(cartas: List[String]): Boolean = {
	  if(cartas.length == 2) (cartas.count(c => getValor(c) == getValor(cartas.head)) == 2)
	  else (cartas.count(c => getValor(c) == getValor(cartas.head)) == 2) ||  JogadaPar(cartas.tail)
	}
		
	/** High Card */
	def JogadaCartaAlta(cartas: List[String]): Int = {
	  getValor(ordenaInverso(cartas).head)
	}
	
	//----------------------------------------------------- Fim Jogadas ---------------------------------------------------------------------------//
	
	/** Testa se as cartas estão ordenadas, não necessariamente precisa estar em sequência */
	def emOrdem(cartas: List[String]): Boolean = {
	  if (cartas.length == 2) { 
	    getValor(cartas.head) <= getValor(cartas.last)
	  } else {
	     (getValor(cartas.head) <= getValor(cartas.tail.head)) && emOrdem(cartas.tail)
	  }
	}
	
	def ordena(cartas: List[String]): List[String] = {
	  cartas.sort((c1, c2) => getValor(c1) < getValor(c2))
	}
	  
	def ordenaInverso(cartas: List[String]): List[String] = {
	  cartas.sort((c1, c2) => getValor(c1) > getValor(c2))
	}
			
	/** Retorna o valor da carta */
	def getValor(carta: String): Int = {
	  carta.substring(0, carta.length() - 1).toInt
	}
}
