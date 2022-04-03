import XCTest
import Marpa


private extension TestGrammar {
  func isNullable(_ s: Symbol) -> Bool {
    return [top, a1, a2, b1, b2, c1, c2].contains(s)
  }

  init() {
    self.init(nullableCs: true)
    g.startSymbol = top
    XCTAssertEqual(g.startSymbol, top)
  }
}

/// Test case mirroring trivial1.c from the libMarpa repo.
final class Trivial1: XCTestCase {
  func test1() {
    let g0 = TestGrammar()
    g0.g.precompute()
    let g = g0.g
    let (top, a1, a2, c2) = (g0.top, g0.a1, g0.a2, g0.c2)
    let (top1, top2, c2_1) = (g0.top1, g0.top2, g0.c2_1!)

    XCTAssertEqual(g.allSymbols.count, Int(c2.id + 1))
    
    // Before precomputation
    XCTAssertEqual(g.allRules.count, Int(c2_1.id + 1))
    XCTAssertEqual(g.rhsCount(top1), 1)
    XCTAssertEqual(g.rhsCount(c2_1), 0)
    XCTAssertEqual(g.lhs(top1), g0.top)
    XCTAssert(g.rhs(top1).elementsEqual([a1]))
    XCTAssert(g.rhs(top2).elementsEqual([a2]))
    XCTAssert(g.rhs(top2).elementsEqual([a2]))
    
    /* Symbols -- status accessors must succeed on precomputed grammar */
    XCTAssert(g.isAccessible(c2))
    XCTAssert(g.isNullable(a1))
    XCTAssert(g.isNulling(a1))
    XCTAssert(g.isProductive(top))

    XCTAssertFalse(g.isTerminal(top))
    
    /* Rules */
    XCTAssert(g.isAccessible(top1))
    XCTAssert(g.isNullable(top2))
    XCTAssert(g.isNulling(top2))
    XCTAssertFalse(g.isLoop(c2_1))
    
    XCTAssertEqual(g.rhsCount(top1), 1)
    XCTAssertEqual(g.rhs(top1).count, 1)
    
    XCTAssertEqual(g.rhsCount(c2_1), 0)
    XCTAssertEqual(g.rhs(c2_1).count, 0)
    
    XCTAssert(g.isProductive(c2_1))
    XCTAssertEqual(g.lhs(top1), top)

    XCTAssert(g.rhs(top1).elementsEqual([a1]))
    XCTAssert(g.rhs(top2).elementsEqual([a2]))

    XCTAssertFalse(g.isProperSeparation(top1))
    XCTAssertFalse(g.isCountedInSequence(top))
  }

  func test2() {
    let g0 = TestGrammar()
    let g = g0.g
    let (top1, top2) = (g0.top1, g0.top2)
    
    /* Ranks */
    g.rank[top1] = -2
    XCTAssertEqual(g.rank[top1], -2)
    
    g.rank[top2] = 2
    XCTAssertEqual(g.rank[top2], 2)

    g.nullRanksHigh[top2] = true
    XCTAssert(g.nullRanksHigh[top2])
    
    g0.g.precompute()
    
    /* getters succeed */
    XCTAssertEqual(g.rank[top1], -2)
    XCTAssertEqual(g.rank[top2], 2)
    XCTAssert(g.nullRanksHigh[top2])
  }

  func test3() {
    let g0 = TestGrammar()
    let g = g0.g
    let (top, a1, a2, b1, b2, c1, c2)
      = (g0.top, g0.a1, g0.a2, g0.b1, g0.b2, g0.c1, g0.c2)
    let (top1, top2, c2_1) = (g0.top1, g0.top2, g0.c2_1!)

    /* completion */
    let completed = b1
    g.canTriggerCompletionEvent[completed] = false
    XCTAssertFalse(g.canTriggerCompletionEvent[completed])
    g.canTriggerCompletionEvent[completed] = true
    XCTAssert(g.canTriggerCompletionEvent[completed])
    g.disableCompletionEvent(completed)
    g.enableCompletionEvent(completed)


    /* prediction */
    let predicted = a1
    g.canTriggerPredictionEvent[predicted] = false
    XCTAssertFalse(g.canTriggerPredictionEvent[predicted])
    g.canTriggerPredictionEvent[predicted] = true
    XCTAssert(g.canTriggerPredictionEvent[predicted])
    g.disablePredictionEvent(predicted)
    g.enablePredictionEvent(predicted)

    /* completion on predicted symbol */
    g.canTriggerCompletionEvent[predicted] = true
    XCTAssert(g.canTriggerCompletionEvent[predicted])
    
    /* prediction on completed symbol */
    g.canTriggerPredictionEvent[completed] = true
    XCTAssert(g.canTriggerPredictionEvent[completed])

    /* precomputation */
    g.precompute()

    
    /* Recognizer Methods */
    do {
      let r = Recognizer(g);
      r.startInput()
      
      /* start the recce */
      // The below recce tests are at earleme 0

      /* event loop -- just count events so far -- there must be no event except exhausted */
      XCTAssert(g.events.elementsEqual([.parseExhausted]))
      
      // at earleme 0
      XCTAssert(r.isExhausted)

      /* Location Accessors */
      do {
        /* the below 2 always succeed */
        XCTAssertEqual(r.currentEarleme?.id, 0)
        XCTAssertEqual(r.furthestEarleme, r.currentEarleme)
        
        XCTAssertEqual(r.latestEarleySet.id, r.furthestEarleme.id)

        XCTAssertEqual(r.earleme(r.latestEarleySet), r.currentEarleme)

        r.setValueForLatestEarleySet((-2, nil))
        
        var addressable = 0
        withUnsafeMutablePointer(to: &addressable) { address in 
          r.setValueForLatestEarleySet((42, .init(address)))
          XCTAssert(r.value(for: r.latestEarleySet) == (42, .init(address)))
        }
      }
      
      let earleme0 = r.currentEarleme
      XCTAssertEqual(earleme0?.id, 0)
      let earleySet0 = r.latestEarleySet
      XCTAssertEqual(earleySet0.id, 0)
      
      /* Other parse status methods */
      do 
      {
        r.disablePredictionEvent(predicted)
        r.enableCompletionEvent(completed)
        let nulled = c1
        r.enableNulledEvent(nulled)

        r.earleyItemWarningThreshold = 1
        XCTAssertEqual(r.earleyItemWarningThreshold, 1)

        let expected = c1
        XCTAssertEqual(r.expectedTerminals.count, 0)
        XCTAssertFalse(r.expects(c1))

      }

      /* Progress reports */
      do {
        XCTAssertNil(r.progress(at: r.latestEarleySet).next())
      }

      /* Bocage, Order, Tree, Value */
      do {
        /* Bocage */
        do {
          guard let b = Bocage(r) else {
            XCTFail("unexpected null Bocage")
            return
          }
        }

        guard let b = Bocage(r, endPosition: earleySet0) else {
          XCTFail("unexpected null Bocage")
          return
        }
        XCTAssertFalse(b.isAmbiguous)
        XCTAssert(b.isNull)

        /* Order */
        let o = Order(b)
        XCTAssert(o.containsHighRankTreesOnly)
        XCTAssertFalse(o.isAmbiguous)
        XCTAssert(o.isNull)
        XCTAssert(o.containsHighRankTreesOnly)
        
        /* Tree */
        var t = o.makeIterator()

        /* Value */
        guard var v = t.next() else {
          XCTFail("Unexpectedly found no evaluation")
          return
        }
        var inactiveCount = 0

        XCTAssertNil(v.next())
  
      }
    }
  }
}

