// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie

package object specs2 {

  type Checker[M[_]] = analysisspec.Checker[M]
  type IOChecker = analysisspec.IOChecker
  type AnalysisMatchers[M[_]] = analysismatchers.AnalysisMatchers[M]
  type IOAnalysisMatchers = analysismatchers.IOAnalysisMatchers

}
