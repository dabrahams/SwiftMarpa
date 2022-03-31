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
    g.precompute()
  }
}

/// Test case mirroring trivial1.c from the libMarpa repo.
final class Trivial1: XCTestCase {
  func test() {
    let g0 = TestGrammar()
    let g = g0.g
    let (top, a1, a2, b1, b2, c1, c2)
      = (g0.top, g0.a1, g0.a2, g0.b1, g0.b2, g0.c1, g0.c2)
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
    /*    

  /* Sequences */

  /* non-sequence rule id */
  API_STD_TEST1(defaults, 0, MARPA_ERR_NONE, marpa_g_rule_is_proper_separation, g, R_top_1);
  API_STD_TEST1(defaults, -1, MARPA_ERR_NOT_A_SEQUENCE, marpa_g_sequence_min, g, R_top_1);
  API_STD_TEST1(defaults, -2, MARPA_ERR_NOT_A_SEQUENCE, marpa_g_sequence_separator, g, R_top_1);
  API_STD_TEST1(defaults, 0, MARPA_ERR_NONE, marpa_g_symbol_is_counted, g, S_top);

  /* invalid/no such rule id error handling */

  /* Sequence mutator methods */
  API_STD_TEST1(defaults, -2, MARPA_ERR_INVALID_RULE_ID, marpa_g_sequence_separator, g, R_invalid);
  API_STD_TEST1(defaults, -2, MARPA_ERR_INVALID_RULE_ID, marpa_g_sequence_min, g, R_invalid);

  /* Sequence mutator methods */
  API_STD_TEST1(defaults, -2, MARPA_ERR_NO_SUCH_RULE_ID, marpa_g_sequence_separator, g, R_no_such);
  API_STD_TEST1(defaults, -2, MARPA_ERR_NO_SUCH_RULE_ID, marpa_g_sequence_min, g, R_no_such);

  API_STD_TEST1(defaults, -2, MARPA_ERR_INVALID_RULE_ID, marpa_g_rule_is_proper_separation, g, R_invalid);
  API_STD_TEST1(defaults, -1, MARPA_ERR_NO_SUCH_RULE_ID, marpa_g_rule_is_proper_separation, g, R_no_such);

  API_STD_TEST1(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID, marpa_g_symbol_is_counted, g, S_invalid);
  API_STD_TEST1(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID, marpa_g_symbol_is_counted, g, S_no_such);

  /* Ranks */
  negative_rank = -2;
  API_HIDDEN_TEST2(defaults, negative_rank, MARPA_ERR_NONE,
      marpa_g_rule_rank_set, g, R_top_1, negative_rank);
  API_HIDDEN_TEST1(defaults, negative_rank, MARPA_ERR_NONE,
      marpa_g_rule_rank, g, R_top_1);

  positive_rank = 2;
  API_HIDDEN_TEST2(defaults, positive_rank, MARPA_ERR_NONE,
      marpa_g_rule_rank_set, g, R_top_2, positive_rank);
  API_HIDDEN_TEST1(defaults, positive_rank, MARPA_ERR_NONE,
      marpa_g_rule_rank, g, R_top_2);

  flag = 1;
  API_HIDDEN_TEST2(defaults, flag, MARPA_ERR_NONE,
      marpa_g_rule_null_high_set, g, R_top_2, flag);
  API_HIDDEN_TEST1(defaults, flag, MARPA_ERR_NONE,
      marpa_g_rule_null_high, g, R_top_2);

  /* invalid/no such rule id error handling */

  /* Rank setter methods */
  API_HIDDEN_TEST2(defaults, -2, MARPA_ERR_INVALID_RULE_ID,
      marpa_g_rule_rank_set, g, R_invalid, negative_rank);

  API_HIDDEN_TEST2(defaults, -2, MARPA_ERR_INVALID_RULE_ID,
      marpa_g_rule_null_high_set, g, R_invalid, whatever);

  API_HIDDEN_TEST2(defaults, -2, MARPA_ERR_NO_SUCH_RULE_ID,
      marpa_g_rule_rank_set, g, R_no_such, negative_rank);

  API_HIDDEN_TEST2(defaults, -1, MARPA_ERR_NO_SUCH_RULE_ID,
      marpa_g_rule_null_high_set, g, R_no_such, whatever);

  /* Rank getter methods */
  API_HIDDEN_TEST1(defaults, -2, MARPA_ERR_INVALID_RULE_ID,
      marpa_g_rule_rank, g, R_invalid);

  API_HIDDEN_TEST1(defaults, -2, MARPA_ERR_INVALID_RULE_ID,
      marpa_g_rule_null_high, g, R_invalid);

  API_HIDDEN_TEST1(defaults, -2, MARPA_ERR_INVALID_RULE_ID,
      marpa_g_rule_null_high, g, R_invalid);
  API_HIDDEN_TEST1(defaults, -1, MARPA_ERR_NO_SUCH_RULE_ID,
      marpa_g_rule_null_high, g, R_no_such);

  marpa_g_trivial_precompute(g, S_top);
  ok(1, "precomputation succeeded");

  /* Ranks methods on precomputed grammar */
  /* setters fail */
  API_HIDDEN_TEST2(defaults, -2, MARPA_ERR_PRECOMPUTED,
      marpa_g_rule_rank_set, g, R_top_1, negative_rank);
  API_HIDDEN_TEST2(defaults, -2, MARPA_ERR_PRECOMPUTED,
      marpa_g_rule_rank_set, g, R_top_2, negative_rank);

  API_HIDDEN_TEST2(defaults, -2, MARPA_ERR_PRECOMPUTED,
      marpa_g_rule_null_high_set, g, R_top_2, flag);

  /* getters succeed */
  API_HIDDEN_TEST1(defaults, negative_rank, MARPA_ERR_NONE,
      marpa_g_rule_rank, g, R_top_1);
  API_HIDDEN_TEST1(defaults, positive_rank, MARPA_ERR_NONE,
      marpa_g_rule_rank, g, R_top_2);

  API_HIDDEN_TEST1(defaults, flag, MARPA_ERR_NONE,
      marpa_g_rule_null_high, g, R_top_2);

  /* recreate the grammar to test event methods except nulled */
  marpa_g_unref(g);
  g = marpa_g_trivial_new(&marpa_configuration);

  /* Events */
  /* test that attempts to create events, other than nulled events,
     results in a reasonable error -- http://irclog.perlgeek.de/marpa/2015-02-13#i_10111838 */
  int reactivate;
  int value;
  Marpa_Symbol_ID S_predicted, S_completed;

  /* completion */
  S_completed = S_B1;

  value = 0;
  API_STD_TEST2(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_completion_event_set, g, S_completed, value);
  API_STD_TEST1(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_completion_event, g, S_completed);

  value = 1;
  API_STD_TEST2(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_completion_event_set, g, S_completed, value);
  API_STD_TEST1(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_completion_event, g, S_completed);

  reactivate = 0;
  API_STD_TEST2(defaults, reactivate, MARPA_ERR_NONE,
     marpa_g_completion_symbol_activate, g, S_completed, reactivate);

  reactivate = 1;
  API_STD_TEST2(defaults, reactivate, MARPA_ERR_NONE,
     marpa_g_completion_symbol_activate, g, S_completed, reactivate);

  /* prediction */
  S_predicted = S_A1;

  value = 0;
  API_STD_TEST2(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_prediction_event_set, g, S_predicted, value);
  API_STD_TEST1(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_prediction_event, g, S_predicted);

  value = 1;
  API_STD_TEST2(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_prediction_event_set, g, S_predicted, value);
  API_STD_TEST1(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_prediction_event, g, S_predicted);

  reactivate = 0;
  API_STD_TEST2(defaults, reactivate, MARPA_ERR_NONE,
     marpa_g_prediction_symbol_activate, g, S_predicted, reactivate);

  reactivate = 1;
  API_STD_TEST2(defaults, reactivate, MARPA_ERR_NONE,
     marpa_g_completion_symbol_activate, g, S_predicted, reactivate);

  /* completion on predicted symbol */
  value = 1;
  API_STD_TEST2(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_completion_event_set, g, S_predicted, value);
  API_STD_TEST1(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_completion_event, g, S_predicted);

  /* prediction on completed symbol */
  value = 1;
  API_STD_TEST2(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_prediction_event_set, g, S_completed, value);
  API_STD_TEST1(defaults, value, MARPA_ERR_NONE,
    marpa_g_symbol_is_prediction_event, g, S_completed);

  /* invalid/no such symbol IDs */

  /* Event setter methods */
  API_STD_TEST2(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID,
    marpa_g_symbol_is_completion_event_set, g, S_invalid, whatever);

  API_STD_TEST2(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID,
     marpa_g_completion_symbol_activate, g, S_invalid, whatever);

  API_STD_TEST2(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID,
    marpa_g_symbol_is_prediction_event_set, g, S_invalid, value);

  API_STD_TEST2(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID,
     marpa_g_prediction_symbol_activate, g, S_invalid, whatever);

  /* Event setter methods */
  API_STD_TEST2(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID,
    marpa_g_symbol_is_completion_event_set, g, S_no_such, whatever);

  API_STD_TEST2(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID,
     marpa_g_completion_symbol_activate, g, S_no_such, whatever);

  API_STD_TEST2(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID,
    marpa_g_symbol_is_prediction_event_set, g, S_no_such, whatever);

  API_STD_TEST2(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID,
     marpa_g_prediction_symbol_activate, g, S_no_such, whatever);

  /* Event getter methods */
  API_STD_TEST1(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID, marpa_g_symbol_is_completion_event, g, S_invalid);
  API_STD_TEST1(defaults, -2, MARPA_ERR_INVALID_SYMBOL_ID, marpa_g_symbol_is_prediction_event, g, S_invalid);

  /* Event getter methods */
  API_STD_TEST1(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID, marpa_g_symbol_is_completion_event, g, S_no_such);
  API_STD_TEST1(defaults, -1, MARPA_ERR_NO_SUCH_SYMBOL_ID, marpa_g_symbol_is_prediction_event, g, S_no_such);

  /* precomputation */
  marpa_g_trivial_precompute(g, S_top);
  ok(1, "precomputation succeeded");

  /* event methods after precomputation */
  /* Event setter methods */
  API_STD_TEST2(defaults, -2, MARPA_ERR_PRECOMPUTED,
    marpa_g_symbol_is_completion_event_set, g, whatever, whatever);

  API_STD_TEST2(defaults, -2, MARPA_ERR_PRECOMPUTED,
     marpa_g_completion_symbol_activate, g, S_no_such, whatever);

  API_STD_TEST2(defaults, -2, MARPA_ERR_PRECOMPUTED,
    marpa_g_symbol_is_prediction_event_set, g, S_predicted, whatever);

  API_STD_TEST2(defaults, -2, MARPA_ERR_PRECOMPUTED,
     marpa_g_prediction_symbol_activate, g, S_no_such, whatever);

  API_STD_TEST1(defaults, value, MARPA_ERR_NO_SUCH_SYMBOL_ID, marpa_g_symbol_is_prediction_event, g, S_predicted);
  API_STD_TEST1(defaults, value, MARPA_ERR_NO_SUCH_SYMBOL_ID, marpa_g_symbol_is_completion_event, g, S_completed);

  /* Recognizer Methods */
  {
    r = marpa_r_new (g);
    if (!r)
      fail("marpa_r_new", g);

    /* the recce hasn't been started yet */

    API_STD_TEST0(defaults, -1, MARPA_ERR_RECCE_NOT_STARTED, marpa_r_current_earleme, r);
    API_STD_TEST0(defaults, -2, MARPA_ERR_RECCE_NOT_STARTED, marpa_r_progress_report_reset, r);
    API_STD_TEST1(defaults, -2, MARPA_ERR_RECCE_NOT_STARTED, marpa_r_progress_report_start, r, whatever);
    API_STD_TEST0(defaults, -2, MARPA_ERR_RECCE_NOT_STARTED, marpa_r_progress_report_finish, r);

    {
      int set_id;
      Marpa_Earley_Set_ID origin;
      API_STD_TEST2(defaults, -2, MARPA_ERR_RECCE_NOT_STARTED,
	marpa_r_progress_item, r, &set_id, &origin);
    }

    /* start the recce */
    rc = marpa_r_start_input (r);
    if (!rc)
      fail("marpa_r_start_input", g);

    diag ("The below recce tests are at earleme 0");

    { /* event loop -- just count events so far -- there must be no event except exhausted */
      Marpa_Event event;
      int exhausted_event_triggered = 0;
      int spurious_events = 0;
      int prediction_events = 0;
      int completion_events = 0;
      int event_ix;
      const int event_count = marpa_g_event_count (g);

      is_int(1, event_count, "event count at earleme 0 is %ld", (long) event_count);

      for (event_ix = 0; event_ix < event_count; event_ix++)
      {
        int event_type = marpa_g_event (g, &event, event_ix);
        if (event_type == MARPA_EVENT_SYMBOL_COMPLETED)
          completion_events++;
        else if (event_type == MARPA_EVENT_SYMBOL_PREDICTED)
          prediction_events++;
        else if (event_type == MARPA_EVENT_EXHAUSTED)
          exhausted_event_triggered++;
        else
        {
          printf ("spurious event type is %ld\n", (long) event_type);
          spurious_events++;
        }
      }

      is_int(0, spurious_events, "spurious events triggered: %ld", (long) spurious_events);
      is_int(0, completion_events, "completion events triggered: %ld", (long) completion_events);
      is_int(0, prediction_events, "completion events triggered: %ld", (long) prediction_events);
      ok (exhausted_event_triggered, "exhausted event triggered");

    } /* event loop */

    /* recognizer reading methods */
    Marpa_Symbol_ID S_token = S_A2;
    this_test.msg = "not accepting input is checked before invalid symbol";
    API_CODE_TEST3(this_test, MARPA_ERR_RECCE_NOT_ACCEPTING_INPUT,
        marpa_r_alternative, r, S_invalid, 0, 0);

    this_test.msg = "not accepting input is checked before no such symbol";
    API_CODE_TEST3(this_test, MARPA_ERR_RECCE_NOT_ACCEPTING_INPUT,
        marpa_r_alternative, r, S_no_such, 0, 0);

    this_test.msg = "not accepting input";
    API_CODE_TEST3(this_test, MARPA_ERR_RECCE_NOT_ACCEPTING_INPUT,
        marpa_r_alternative, r, S_token, 0, 0);

    this_test = defaults;

    API_STD_TEST0(defaults, -2, MARPA_ERR_RECCE_NOT_ACCEPTING_INPUT,
	marpa_r_earleme_complete, r);

    this_test.msg = "at earleme 0";
    API_STD_TEST0(this_test, 1, MARPA_ERR_NONE, marpa_r_is_exhausted, r);

    /* Location accessors */
    {
      /* the below 2 always succeed */
      unsigned int current_earleme = 0;
      API_STD_TEST0(defaults, current_earleme, MARPA_ERR_NONE, marpa_r_current_earleme, r);

      unsigned int furthest_earleme = current_earleme;
      API_STD_TEST0(defaults, furthest_earleme, MARPA_ERR_NONE, marpa_r_furthest_earleme, r);

      API_STD_TEST0(defaults, furthest_earleme, MARPA_ERR_NONE, marpa_r_latest_earley_set, r);

      API_STD_TEST1(defaults, current_earleme, MARPA_ERR_NONE,
	marpa_r_earleme, r, current_earleme);

      API_STD_TEST1(defaults, -1, MARPA_ERR_RECCE_NOT_ACCEPTING_INPUT,
	marpa_r_earley_set_value, r, current_earleme);

      /* marpa_r_earley_set_value_*() methods */
      int taxicab = 1729;

      int earley_set;

      struct marpa_r_earley_set_value_test {
        int earley_set;

        int rv_marpa_r_earleme;
        int rv_marpa_r_latest_earley_set_value_set;
        int rv_marpa_r_earley_set_value;
        int rv_marpa_r_latest_earley_set_values_set;

        int   rv_marpa_r_earley_set_values;
        int   int_p_value_rv_marpa_r_earley_set_values;
        void* void_p_value_rv_marpa_r_earley_set_values;

        Marpa_Error_Code errcode;
      };
      typedef struct marpa_r_earley_set_value_test Marpa_R_Earley_Set_Value_Test;

      const Marpa_R_Earley_Set_Value_Test tests[] = {
        { -1, -2, taxicab,      -2, 1, -2, taxicab, value2, MARPA_ERR_INVALID_LOCATION },
        {  0,  0, taxicab, taxicab, 1,  1,      42, value2, MARPA_ERR_INVALID_LOCATION },
        {  1, -2,      42,      -2, 1, -2,      42, value2, MARPA_ERR_NO_EARLEY_SET_AT_LOCATION },
        {  2, -2,      42,      -2, 1, -2,      42, value2, MARPA_ERR_NO_EARLEY_SET_AT_LOCATION },
      };

      for (ix = 0; ix < sizeof(tests) / sizeof(Marpa_R_Earley_Set_Value_Test); ix++)
        {
          const Marpa_R_Earley_Set_Value_Test t = tests[ix];
          diag("marpa_r_earley_set_value_*() methods, earley_set: %d", t.earley_set);

          if (t.earley_set == -1 || t.earley_set == 1 || t.earley_set == 2) {
	    API_STD_TEST1(defaults, t.rv_marpa_r_earleme, t.errcode,
	      marpa_r_earleme, r, t.earley_set);
          } else {
	    API_STD_TEST1(defaults, t.rv_marpa_r_earleme, MARPA_ERR_NONE,
	      marpa_r_earleme, r, t.earley_set);
	  }

	  API_STD_TEST1(defaults, t.rv_marpa_r_latest_earley_set_value_set, MARPA_ERR_NONE,
	    marpa_r_latest_earley_set_value_set, r, t.rv_marpa_r_latest_earley_set_value_set);

          if (t.earley_set == -1 || t.earley_set == 1 || t.earley_set == 2) {
	    API_STD_TEST1(defaults, t.rv_marpa_r_earley_set_value, t.errcode,
	      marpa_r_earley_set_value, r, t.earley_set);
          } else {
	    API_STD_TEST1(defaults, t.rv_marpa_r_earley_set_value, MARPA_ERR_NONE,
	      marpa_r_earley_set_value, r, t.earley_set);
	  }

	  {
	    API_STD_TEST2(defaults,
	      t.rv_marpa_r_latest_earley_set_values_set,
	      MARPA_ERR_NONE,
	      marpa_r_latest_earley_set_values_set,
	      r,
	      42, value2);
	  }

	  {
	    /* There is no c89 portable way to test arbitrary pointers.
	     * With ifdef's we could cover 99.999% of cases, but for now
	     * we do not bother.
	     */
	    void *orig_value2 = NULL;
	    void *value2 = orig_value2;

	    API_STD_TEST3 (defaults,
			   t.rv_marpa_r_earley_set_values,
			   t.errcode,
			   marpa_r_earley_set_values,
			   r, t.earley_set, (&int_value), &value2);
	    is_int (t.int_p_value_rv_marpa_r_earley_set_values,
		    int_value, "marpa_r_earley_set_values() int* value");

	  }

        }
    } /* Location Accessors */

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

