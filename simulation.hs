module Simulation (customerArriveProbability, customerProcessMeanTime) where

import Types

e = exp 1

customerArriveProbabilityAlpha = 100

p = 200

customerArriveProbability :: SecondsPassed -> Maybe Probability
customerArriveProbability t | t < 0 = Nothing
customerArriveProbability t = 
    let x = -1 * ((fromIntegral t) / (fromIntegral customerArriveProbabilityAlpha))
    in Just (1 - (e ** x))

customerProcessTime :: CustomerType -> Double -> Maybe CustomerProcessTimeSeconds
customerProcessTime _ x | x < 0 || x > 1 = Nothing
customerProcessTime (CustomerType alpha beta) x = 
    let
        a = (x ^^ (alpha - 1))
        b = (1 - x) ^^ (beta - 1)
    in
        Just (p * a * b)

customerProcessMeanTime :: CustomerType -> Maybe CustomerProcessTimeSeconds
customerProcessMeanTime cType@(CustomerType alpha beta) = 
    let
        meanPoint = (fromIntegral alpha) / (fromIntegral (alpha + beta))
    in
        customerProcessTime cType meanPoint

