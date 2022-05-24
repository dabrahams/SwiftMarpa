import XCTest
import Marpa


fileprivate class Trivial1Grammar: TestGrammar {
  func isKnownNullable(_ s: Symbol) -> Bool {
    return [self.top, self.a1, self.a2, self.b1, self.b2, self.c1, self.c2].contains(s)
  }

  init() {
    super.init(nullableCs: true)
    startSymbol = self.top
    XCTAssertEqual(startSymbol, self.top)
  }
}

/// Test case mirroring trivial1.c from the libMarpa repo.
final class Trivial1: XCTestCase {
  func test1() {
    let g = Trivial1Grammar()
    g.precompute()

    XCTAssertEqual(g.symbols.count, Int(g.c2.id + 1))

    // Before precomputation
    XCTAssertEqual(g.rules.count, Int(g.c2_1.id + 1))
    XCTAssertEqual(g.rhsCount(g.top1), 1)
    XCTAssertEqual(g.rhsCount(g.c2_1), 0)
    XCTAssertEqual(g.lhs(g.top1), g.top)
    XCTAssert(g.rhs(g.top1).elementsEqual([g.a1]))
    XCTAssert(g.rhs(g.top2).elementsEqual([g.a2]))
    XCTAssert(g.rhs(g.top2).elementsEqual([g.a2]))
    
    /* Symbols -- status accessors must succeed on precomputed grammar */
    XCTAssert(g.isAccessible(g.c2))
    XCTAssert(g.isKnownNullable(g.a1))
    XCTAssert(g.isNulling(g.a1))
    XCTAssert(g.isProductive(g.top))

    XCTAssertFalse(g.isTerminal(g.top))
    
    /* Rules */
    XCTAssert(g.isAccessible(g.top1))
    XCTAssert(g.isNullable(g.top2))
    XCTAssert(g.isNulling(g.top2))
    XCTAssertFalse(g.isLoop(g.c2_1))
    
    XCTAssertEqual(g.rhsCount(g.top1), 1)
    XCTAssertEqual(g.rhs(g.top1).count, 1)
    
    XCTAssertEqual(g.rhsCount(g.c2_1), 0)
    XCTAssertEqual(g.rhs(g.c2_1).count, 0)
    
    XCTAssert(g.isProductive(g.c2_1))
    XCTAssertEqual(g.lhs(g.top1), g.top)

    XCTAssert(g.rhs(g.top1).elementsEqual([g.a1]))
    XCTAssert(g.rhs(g.top2).elementsEqual([g.a2]))

    XCTAssertFalse(g.isProperSeparation(g.top1))
    XCTAssertFalse(g.isCountedInSequence(g.top))
  }

  func test2() {
    let g = Trivial1Grammar()

    /* Ranks */
    g.rank[g.top1] = -2
    XCTAssertEqual(g.rank[g.top1], -2)
    
    g.rank[g.top2] = 2
    XCTAssertEqual(g.rank[g.top2], 2)

    g.nullRanksHigh[g.top2] = true
    XCTAssert(g.nullRanksHigh[g.top2])
    
    g.precompute()
    
    /* getters succeed */
    XCTAssertEqual(g.rank[g.top1], -2)
    XCTAssertEqual(g.rank[g.top2], 2)
    XCTAssert(g.nullRanksHigh[g.top2])
  }

  func test3() {
    let g = Trivial1Grammar()

    /* completion */
    let completed: Symbol = g.b1
    g.canTriggerCompletionEvent[completed] = false
    XCTAssertFalse(g.canTriggerCompletionEvent[completed])
    g.canTriggerCompletionEvent[completed] = true
    XCTAssert(g.canTriggerCompletionEvent[completed])
    g.disableCompletionEvent(completed)
    g.enableCompletionEvent(completed)


    /* prediction */
    let predicted: Symbol = g.a1
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
        let nulled = g.c1
        r.enableNulledEvent(nulled)

        r.earleyItemWarningThreshold = 1
        XCTAssertEqual(r.earleyItemWarningThreshold, 1)

        XCTAssertEqual(r.expectedTerminals.count, 0)
        XCTAssertFalse(r.expects(g.c1))

      }

      /* Progress reports */
      do {
        XCTAssertNil(r.progress(at: r.latestEarleySet).next())
      }

      /* Bocage, Order, Tree, Value */
      do {
        /* Bocage */
        do {
          guard Bocage(r) != nil else {
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
        let t = o.makeIterator()

        do {
          /* Value */
          guard let v = t.next() else {
            XCTFail("Unexpectedly found no evaluation")
            return
          }
          guard let step0 = v.next() else {
            XCTFail("expected step not found")
            return
          }
          guard let s = step0.symbol else {
            XCTFail("expected symbol but got \(step0)")
            return
          }
          XCTAssertEqual(s.tokenValue, nil, "expected a nulling symbol")
          XCTAssertEqual(s.output, 0)
          XCTAssertEqual(s.0.id, 0)
          XCTAssertEqual(s.sourceRange.upperBound.id, 0)
          XCTAssertEqual(s.sourceRange.lowerBound.id, 0)

          XCTAssertEqual(v.next(), nil)
          XCTAssertEqual(v.next(), nil)
        }

        // Tree iterator should be unpaused now.
        XCTAssertNil(t.next())
      }
    }
  }
}

