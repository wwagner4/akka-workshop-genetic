package clashcode

import clashcode.robot._
import scala.collection.GenSeq

case object ChrisPopBuildStrategy extends PopulationBuildingStrategy {

  /**
   * Append new members and members of the previos generation. select the fittest
   */
  def createNextPopulation(generation: Int, poolSize: Int,
    newMembers: GenSeq[CandidatePoints], previousGeneration: Seq[CandidatePoints]): Seq[CandidatePoints] = {
    
    val allCandidates = previousGeneration ++ newMembers
    allCandidates.sortBy(-_.points).take(poolSize)
  }

}
