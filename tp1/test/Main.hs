module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador ( genFijo, genNormalConSemilla )
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      -- AGREGADO: rellena string vacío
      alinearDerecha 2 "" ~?= "  ",
      -- AGREGADO: no tiene problema si el input son blancos
      alinearDerecha 2 "   " ~?= "   "
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      -- AGREGADO: actualizar sin efecto (+0, id)
      actualizarElem 1 (+ 0) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem 1 id [1, 2, 3] ~?= [1, 2, 3],
      -- AGREGADO: fuera de índice (lower/upper)
      actualizarElem (-1) (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem 16 (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      -- AGREGADO: lista vacía
      actualizarElem 3 (+ 1) [] ~?= []
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      -- AGREGADO: rango cruzando cero
      casilleros (vacio 2 (-1, 1))
        ~?= [ Casillero infinitoNegativo (-1) 0 0,
              Casillero (-1) 0 0 0,
              Casillero 0 1 0 0,
              Casillero 1 infinitoPositivo 0 0
            ],
      -- AGREGADO: rangos con decimales
      casilleros (vacio 4 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 1.5 0 0,
              Casillero 1.5 3 0 0,
              Casillero 3 4.5 0 0,
              Casillero 4.5 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          -- AGREGADO: prueba con un valor +infinito
          casilleros (agregar infinitoPositivo h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 100 -- El 100% de los valores están acá
                ],
          -- AGREGADO: prueba con un valor +infinito
          casilleros (agregar 1 (agregar 5 h0))
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 50.0, -- El 50% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 1 50.0, -- El 50% de los valores están acá
                  Casillero 6 infinitoPositivo 0 0
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      -- AGREGADO: histograma vacío
      histograma 4 (1, 5) [] ~?= vacio 4 (1, 5),
      -- AGREGADO: histograma con -infinito
      histograma 4 (1, 5) [infinitoNegativo] ~?= agregar infinitoNegativo (vacio 4 (1, 5)),
      -- AGREGADO: histograma con +infinito
      histograma 4 (1, 5) [infinitoPositivo] ~?= agregar infinitoPositivo (vacio 4 (1, 5))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      -- AGREGADO: casilleros de histograma con datos
      casilleros (histograma 3 (0, 6) [1, 2, 3, 7, -2, 0, infinitoPositivo, -0.1])
        ~?= [ Casillero infinitoNegativo 0.0 2 25.0,
              Casillero 0.0 2.0 2 25.0,
              Casillero 2.0 4.0 2 25.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 2 25.0
            ],
      -- AGREGADO: casilleros de histograma vacío
      casilleros (histograma 3 (0, 6) [])
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ]
    ]

testsRecr :: Test
testsRecr =
  let recProc = recrExpr
                  -- fConst
                  (\v -> show v)
                  -- fRange
                  (\s e -> show s ++ " ~ " ++ show e)
                  -- fSuma
                  (\exprL exprR l r -> "(" ++ l ++ " + " ++ r ++ ")")
                  -- fResta
                  (\exprL exprR l r -> "(" ++ l ++ " - " ++ r ++ ")")
                  -- fMult
                  (\exprL exprR l r -> "(" ++ l ++ " * " ++ r ++ ")")
                  -- fDiv
                  (\exprL exprR l r -> "(" ++ l ++ " / " ++ r ++ ")")
    in test
    [ 
      -- usamos recursión primitiva para convertir una Expr a string
      recProc (Const 1.0) ~?= "1.0",
      recProc (Rango (-1.7) (-0.5)) ~?= "-1.7 ~ -0.5",
      recProc (Suma (Const 1.0) (Const 2.0)) ~?= "(1.0 + 2.0)",
      recProc (Div (Const 1.0) (Const 2.0)) ~?= "(1.0 / 2.0)",
      recProc (Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0))) ~?= "(1.0 + (2.0 * 3.0))",
      recProc (Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0)) ~?= "((1.0 + 2.0) + 3.0)",
      recProc (Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0))) ~?= "(1.0 + (2.0 + 3.0))",
      recProc (Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0)) ~?= "((1.0 + 2.0 ~ 3.0) + 4.0)",
      recProc (Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0)) ~?= "(((1.0 - 2.0) - 3.0) - 4.0)",
      recProc (Resta (Mult (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0)) ~?= "(((1.0 - 2.0) * 3.0) - 4.0)"
    ]

testsFold :: Test
testsFold =
  let foldProc = foldExpr
                  -- fConst
                  (\ v -> show v)
                  -- fRange
                  (\ s e -> show s ++ " ~ " ++ show e)
                  -- fSuma
                  (\ l r -> "(" ++ l ++ " + " ++ r ++ ")")
                  -- fResta
                  (\ l r -> "(" ++ l ++ " - " ++ r ++ ")")
                  -- fMult
                  (\ l r -> "(" ++ l ++ " * " ++ r ++ ")")
                  -- fDiv
                  (\ l r -> "(" ++ l ++ " / " ++ r ++ ")")
    in test
    [ 
      -- usamos recursión estructural para convertir una Expr a string
      foldProc (Const 1.0) ~?= "1.0",
      foldProc (Rango (-1.7) (-0.5)) ~?= "-1.7 ~ -0.5",
      foldProc (Suma (Const 1.0) (Const 2.0)) ~?= "(1.0 + 2.0)",
      foldProc (Div (Const 1.0) (Const 2.0)) ~?= "(1.0 / 2.0)",
      foldProc (Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0))) ~?= "(1.0 + (2.0 * 3.0))",
      foldProc (Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0)) ~?= "((1.0 + 2.0) + 3.0)",
      foldProc (Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0))) ~?= "(1.0 + (2.0 + 3.0))",
      foldProc (Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0)) ~?= "((1.0 + 2.0 ~ 3.0) + 4.0)",
      foldProc (Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0)) ~?= "(((1.0 - 2.0) - 3.0) - 4.0)",
      foldProc (Resta (Mult (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0)) ~?= "(((1.0 - 2.0) * 3.0) - 4.0)"
    ]

testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      -- test suma
      fst (eval (Suma (Const 0) (Const 0)) genFijo) ~?= 0,
      fst (eval (Suma (Const 0) (Const 1)) genFijo) ~?= 1,
      fst (eval (Suma (Const 17) (Const (-20))) genFijo) ~?= (-3),
      -- test resta
      fst (eval (Resta (Const 0) (Const 0)) genFijo) ~?= 0,
      fst (eval (Resta (Const 1) (Const 0)) genFijo) ~?= 1,
      fst (eval (Resta (Const 0) (Const 1)) genFijo) ~?= (-1),
      fst (eval (Resta (Const 17) (Const (-20))) genFijo) ~?= 37,
      -- test mult
      fst (eval (Mult (Const 0) (Const 0)) genFijo) ~?= 0,
      fst (eval (Mult (Const 1) (Const 0)) genFijo) ~?= 0,
      fst (eval (Mult (Const 0) (Const 1)) genFijo) ~?= 0,
      fst (eval (Mult (Const 1) (Const 1)) genFijo) ~?= 1,
      fst (eval (Mult (Const 1) (Const (-1))) genFijo) ~?= (-1),
      fst (eval (Mult (Const 15) (Const 3)) genFijo) ~?= 45,
      -- test div
      fst (eval (Div (Const 0) (Const 0)) genFijo) ~?= infinitoPositivo,
      fst (eval (Div (Const 1) (Const 0)) genFijo) ~?= infinitoPositivo,
      fst (eval (Div (Const (-1)) (Const 0)) genFijo) ~?= infinitoNegativo,
      fst (eval (Div (Const 10) (Const 5)) genFijo) ~?= 2,
      fst (eval (Div (Const 10) (Const 3)) genFijo) ~?= 3.3333333,
      fst (eval (Div (Const 10) (Const 100)) genFijo) ~?= 0.1,
      -- test expresión compleja
      fst (eval (Div (Mult (Const 70) (Resta (Const 8) (Suma (Const 4) (Const 2)))) (Rango 99 101)) genFijo) ~?= 1.4
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [
      casilleros (fst (armarHistograma 2 10 (eval (Const 1)) genFijo)) ~?= 
            [
              Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 1.0 0 0.0,
              Casillero 1.0 2.0 10 100.0,
              Casillero 2.0 infinitoPositivo 0 0.0
            ],
      casilleros (fst (armarHistograma 5 20 (eval (Rango (-2) 2)) (genNormalConSemilla 0) )) ~?= 
            [
              Casillero infinitoNegativo (-2.0378144) 0 0.0,
              Casillero (-2.0378144) (-1.1744351) 3 15.000001,
              Casillero (-1.1744351) (-0.31105578) 3 15.000001,
              Casillero (-0.31105578) 0.5523236 6 30.000002,
              Casillero 0.5523236 1.4157028 5 25.0,
              Casillero 1.4157028 2.279082 2 10.0,
              Casillero 2.279082 infinitoPositivo 1 5.0
            ]
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [
      casilleros (fst (evalHistograma 2 10 (Const 1) genFijo) ) ~?= 
            [
              Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 1.0 0 0.0,
              Casillero 1.0 2.0 10 100.0,
              Casillero 2.0 infinitoPositivo 0 0.0
            ],
      casilleros (fst (evalHistograma 5 20 (Rango (-2) 2) (genNormalConSemilla 0) ) ) ~?= 
            [
              Casillero infinitoNegativo (-2.0378144) 0 0.0,
              Casillero (-2.0378144) (-1.1744351) 3 15.000001,
              Casillero (-1.1744351) (-0.31105578) 3 15.000001,
              Casillero (-0.31105578) 0.5523236 6 30.000002,
              Casillero 0.5523236 1.4157028 5 25.0,
              Casillero 1.4157028 2.279082 2 10.0,
              Casillero 2.279082 infinitoPositivo 1 5.0
            ]
    ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
