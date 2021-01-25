object TopKFrequentWords {
    def topKFrequent(words: Array[String], k: Int): List[String] = {
        val wc = words.foldLeft(Map.empty[String, Int]) ( (p,c) => 
            p + ( c -> (p.getOrElse(c, 0)+1) ) )
        return wc.toList.sortBy(s =>  (-s._2,s._1 ) ).take(k).map(t=>t._1)
    }
}
