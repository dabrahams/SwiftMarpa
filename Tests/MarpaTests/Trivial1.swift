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
/*
    /* Other parse status methods */
    {
      int boolean = 0;
      API_STD_TEST2(defaults, boolean, MARPA_ERR_NONE,
         marpa_r_prediction_symbol_activate, r, S_predicted, boolean);
      API_STD_TEST2(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID,
         marpa_r_prediction_symbol_activate, r, S_invalid, boolean);
      API_STD_TEST2(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID,
         marpa_r_prediction_symbol_activate, r, S_no_such, boolean);

      reactivate = 1;
      API_STD_TEST2(defaults, reactivate, MARPA_ERR_NONE,
         marpa_r_completion_symbol_activate, r, S_completed, reactivate);
      API_STD_TEST2(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID,
         marpa_r_completion_symbol_activate, r, S_invalid, reactivate);
      API_STD_TEST2(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID,
         marpa_r_completion_symbol_activate, r, S_no_such, reactivate);

      boolean = 1;
      Marpa_Symbol_ID S_nulled = S_C1;
      API_STD_TEST2(defaults, boolean, MARPA_ERR_NONE,
         marpa_r_nulled_symbol_activate, r, S_nulled, boolean);
      API_STD_TEST2(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID,
         marpa_r_nulled_symbol_activate, r, S_invalid, boolean);
      API_STD_TEST2(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID,
         marpa_r_nulled_symbol_activate, r, S_no_such, boolean);

      int threshold = 1;
      API_STD_TEST1(defaults, threshold, MARPA_ERR_NONE,
	marpa_r_earley_item_warning_threshold_set, r, threshold);

      API_STD_TEST0(defaults, threshold, MARPA_ERR_NONE, marpa_r_earley_item_warning_threshold, r);

      Marpa_Symbol_ID S_expected = S_C1;
      value = 1;
      API_STD_TEST2(defaults, -2, MARPA_ERR_SYMBOL_IS_NULLING, marpa_r_expected_symbol_event_set, r, S_B1, value);

      {
	Marpa_Symbol_ID buffer[42];
	API_STD_TEST1(defaults, 0, MARPA_ERR_NONE, marpa_r_terminals_expected, r, buffer);
      }

      API_STD_TEST1(defaults, 0, MARPA_ERR_NONE,
	marpa_r_terminal_is_expected, r, S_C1);
      API_STD_TEST1(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID,
	marpa_r_terminal_is_expected, r, S_invalid);
      API_STD_TEST1(defaults, -2, MARPA_ERR_NO_SUCH_SYMBOL_ID,
	marpa_r_terminal_is_expected, r, S_no_such);

    } /* Other parse status methods */

    /* Progress reports */
    {
      API_STD_TEST0(defaults, -2, MARPA_ERR_PROGRESS_REPORT_NOT_STARTED,
          marpa_r_progress_report_reset, r);

      API_STD_TEST0(defaults, -2, MARPA_ERR_PROGRESS_REPORT_NOT_STARTED,
          marpa_r_progress_report_finish, r);

      {
	int set_id;
	Marpa_Earley_Set_ID origin;
	API_STD_TEST2(defaults, -2, MARPA_ERR_PROGRESS_REPORT_NOT_STARTED,
	  marpa_r_progress_item, r, &set_id, &origin);
      }


      /* start report at bad locations */
      Marpa_Earley_Set_ID ys_id_negative = -1;
      API_STD_TEST1(defaults, -2, MARPA_ERR_INVALID_LOCATION,
          marpa_r_progress_report_start, r, ys_id_negative);

      Marpa_Earley_Set_ID ys_id_not_existing = 1;
      API_STD_TEST1(defaults, -2, MARPA_ERR_NO_EARLEY_SET_AT_LOCATION,
          marpa_r_progress_report_start, r, ys_id_not_existing);

      /* start report at earleme 0 */
      Marpa_Earley_Set_ID earleme_0 = 0;
      this_test.msg = "no items at earleme 0";
      API_STD_TEST1(this_test, 0, MARPA_ERR_NONE,
          marpa_r_progress_report_start, r, earleme_0);

      {
	int set_id;
	Marpa_Earley_Set_ID origin;
	API_STD_TEST2(defaults, -1, MARPA_ERR_PROGRESS_REPORT_EXHAUSTED,
	  marpa_r_progress_item, r, &set_id, &origin);
      }


      int non_negative_value = 1;
      API_STD_TEST0(this_test, non_negative_value, MARPA_ERR_NONE,
          marpa_r_progress_report_reset, r);

      this_test.msg = "at earleme 0";
      API_STD_TEST0(this_test, non_negative_value, MARPA_ERR_NONE,
          marpa_r_progress_report_finish, r);
    }

    /* Bocage, Order, Tree, Value */
    {
      /* Bocage */
      Marpa_Earley_Set_ID ys_invalid = -2;
      API_PTR_TEST1(defaults, MARPA_ERR_INVALID_LOCATION,
          marpa_b_new, r, ys_invalid);

      Marpa_Earley_Set_ID ys_non_existing = 1;
      API_PTR_TEST1(defaults, MARPA_ERR_NO_PARSE,
          marpa_b_new, r, ys_non_existing);

      Marpa_Earley_Set_ID ys_at_current_earleme = -1;
      Marpa_Bocage b = marpa_b_new(r, ys_at_current_earleme);
      if (!b)
        fail("marpa_b_new", g);
      else
        ok(1, "marpa_b_new(): parse at current earleme of trivial parse");

      marpa_b_unref(b);

      b = marpa_b_new(r, 0);

      if (!b)
        fail("marpa_b_new", g);
      else
        ok(1, "marpa_b_new(): null parse at earleme 0");

      API_STD_TEST0(defaults, 1, MARPA_ERR_NONE,
	  marpa_b_ambiguity_metric, b);
      API_STD_TEST0(defaults, 1, MARPA_ERR_NONE,
	  marpa_b_is_null, b);

      /* Order */
      Marpa_Order o = marpa_o_new (b);

      if (!o)
        fail("marpa_o_new", g);
      else
        ok(1, "marpa_o_new() at earleme 0");

      int flag = 1;
      API_STD_TEST1(defaults, flag, MARPA_ERR_NONE,
	  marpa_o_high_rank_only_set, o, flag);
      API_STD_TEST0(defaults, flag, MARPA_ERR_NONE,
	  marpa_o_high_rank_only, o);

      API_STD_TEST0(defaults, 1, MARPA_ERR_NONE,
	  marpa_o_ambiguity_metric, o);
      API_STD_TEST0(defaults, 1, MARPA_ERR_NONE,
	  marpa_o_is_null, o);

      API_STD_TEST1(defaults, -2, MARPA_ERR_ORDER_FROZEN,
	  marpa_o_high_rank_only_set, o, flag);
      API_STD_TEST0(defaults, flag, MARPA_ERR_NONE,
	  marpa_o_high_rank_only, o);

      /* Tree */
      Marpa_Tree t;

      t = marpa_t_new (o);
      if (!t)
        fail("marpa_t_new", g);
      else
        ok(1, "marpa_t_new() at earleme 0");

      this_test.msg = "before the first parse tree";
      API_STD_TEST0(this_test, 0, MARPA_ERR_NONE, marpa_t_parse_count, t);
      API_STD_TEST0(defaults, 0, MARPA_ERR_NONE, marpa_t_next, t);

      /* Value */
      Marpa_Value v = marpa_v_new(t);
      if (!t)
        fail("marpa_v_new", g);
      else
        ok(1, "marpa_v_new() at earleme 0");

      int step_inactive_count = 0;
      int step_initial_count = 0;
      int step_token_count = 0;
      int step_rule_count = 0;
      int step_nulling_symbol_count = 0;
      while (1)
      {
        Marpa_Step_Type step_type = marpa_v_step (v);
        Marpa_Symbol_ID token;

        if (step_type < 0)
            fail("marpa_v_step", g);

        if (step_type == MARPA_STEP_INACTIVE)
        {
            step_inactive_count++;
            break;
        }

        switch (step_type)
        {
          case MARPA_STEP_INITIAL:
            step_initial_count++;
            break;
          case MARPA_STEP_TOKEN:
            step_token_count++;
            break;
          case MARPA_STEP_RULE:
            step_rule_count++;
            break;
          case MARPA_STEP_NULLING_SYMBOL:
            step_nulling_symbol_count++;
            break;
         }
      }
      is_int(1, step_inactive_count, "MARPA_STEP_INACTIVE seen once.");
      is_int(0, step_initial_count, "MARPA_STEP_INITIAL not seen.");
      is_int(0, step_token_count, "MARPA_STEP_TOKEN not seen.");
      is_int(0, step_rule_count, "MARPA_STEP_RULE not seen.");
      is_int(0, step_nulling_symbol_count, "MARPA_STEP_NULLING_SYMBOL not seen.");

      API_STD_TEST0(defaults, 1, MARPA_ERR_NONE, marpa_t_parse_count, t);
      API_STD_TEST0(defaults, -2, MARPA_ERR_TREE_PAUSED, marpa_t_next, t);

      marpa_v_unref(v);

      API_STD_TEST0(defaults, 1, MARPA_ERR_NONE, marpa_t_parse_count, t);
      API_STD_TEST0(defaults, -1, MARPA_ERR_TREE_EXHAUSTED, marpa_t_next, t);

    } /* Bocage, Order, Tree, Value */

  } /* recce method tests */
  
 */
  }
  }
}

