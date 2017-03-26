CLASS zcl_sitfra_2017 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS intro_to_table_expr
      RETURNING VALUE(r_sitfra_2017) TYPE REF TO zcl_sitfra_2017.
    METHODS wouldnt_arrays_be_nice
      RETURNING VALUE(r_sitfra_2017) TYPE REF TO zcl_sitfra_2017.
    METHODS fun_with_expressions
      RETURNING VALUE(r_sitfra_2017) TYPE REF TO zcl_sitfra_2017.
    METHODS secondary_indices_are_cool
      RETURNING VALUE(r_sitfra_2017) TYPE REF TO zcl_sitfra_2017.
    METHODS but_there_are_some_caveats
      RETURNING VALUE(r_sitfra_2017) TYPE REF TO zcl_sitfra_2017.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sitfra_2017 IMPLEMENTATION.
  METHOD intro_to_table_expr.
    TYPES:
      BEGIN OF ty_sitfra_talk,
        first_name TYPE string,
        last_name  TYPE string,
        room       TYPE string,
        title      TYPE string,
      END OF ty_sitfra_talk.

    TYPES tt_sitfra_talk TYPE STANDARD TABLE OF ty_sitfra_talk WITH DEFAULT KEY.

    DATA(sitfra_2017_talks)
      = VALUE tt_sitfra_talk(
          (
            first_name = 'Jens'
            last_name = 'Weiler'
            room = 'Room 1'
            title = 'SCN ABAP diary: What the heck is going on?'
          )
          (
            first_name = 'Daniel'
            last_name = 'Ridder'
            room = 'Room 1'
            title = 'ABAP CDS - No Pain no Gain'
          )
          (
            first_name = 'Uwe'
            last_name = 'Fetzer'
            room = 'Room 1'
            title = 'ABAP Magic'
          )
          (
            first_name = 'Lars'
            last_name = 'Hvam'
            room = 'Room 2'
            title = 'Web applications using ABAP'
          )
          (
            first_name = 'Martin'
            last_name = 'Fischer'
            room = 'Room 1'
            title = 'Scrum! But...'
          )
        ).


    DATA(first_talk) = sitfra_2017_talks[ 1 ].

    DATA(uwes_talk) = sitfra_2017_talks[ first_name = 'Uwe' ].
    DATA(lars_talk) = sitfra_2017_talks[ first_name = 'Lars' ].

    r_sitfra_2017 = me.

  ENDMETHOD.


  METHOD wouldnt_arrays_be_nice.
    " On thing that always annoyed my in ABAP is the lack of arrays
    "
    " In ABAP there are only internal table
    "
    " This makes expressing certain algorithms quite complex
    "
    " Example: 2-Dimensional Array
    " ----------------------------
    " |  |  |  |  |  |  |  |  |  |
    " ----------------------------
    " |  |  |  |  |  |  |  |  |  |
    " ----------------------------
    " |  |  |  |  |  |  |  |  |  |
    " ----------------------------
    " |  |  |  |  |  |  |  |  |  |
    " ----------------------------
    " |  |  |  |  |  |  |  |  |  |
    " ----------------------------

    TYPES:
      BEGIN OF ty_cell,
        row   TYPE i,
        col   TYPE i,
        alive TYPE abap_bool,
      END OF ty_cell.

    TYPES tt_grid TYPE HASHED TABLE OF ty_cell WITH UNIQUE KEY row col.

    DATA(grid) =
      VALUE tt_grid(
        FOR i = 0 THEN i + 1 WHILE i < 10
        FOR j = 0 THEN j + 1 WHILE j < 10 (
            row = i
            col = j
            alive = abap_false
        ) ).


    grid[ row = 0 col = 0 ]-alive = abap_true.
    grid[ row = 1 col = 1 ]-alive = abap_true.
    grid[ row = 9 col = 9 ]-alive = abap_true.

    r_sitfra_2017 = me.
  ENDMETHOD.

  METHOD fun_with_expressions.

    " So why are arrays nice?
    "
    " Example: Find the neighbors of a given cell

    " ---------------------
    " |0,0|0,1|0,2|0,3|0,4|
    " ---------------------
    " |1,0|1,1|1,2|1,3|1,4|
    " ---------------------
    " |2,0|2,1|2,2|2,3|2,4|
    " ---------------------
    " |3,0|3,1|3,2|3,3|3,4|
    " ---------------------
    " |4,0|4,1|4,2|4,3|4,4|
    " ---------------------
    TYPES:
      BEGIN OF ty_cell,
        row   TYPE i,
        col   TYPE i,
        alive TYPE abap_bool,
      END OF ty_cell.

    TYPES tt_grid TYPE SORTED TABLE OF ty_cell WITH UNIQUE KEY row col.

    DATA(grid) =
      VALUE tt_grid(
        FOR i = 0 THEN i + 1 WHILE i < 5
        FOR j = 0 THEN j + 1 WHILE j < 5 (
            row = i
            col = j
            alive = abap_false
        ) ).

    "get the neighbors of cell (2,2)
    DATA(row) = 2.
    DATA(col) = 2.

    DATA(neighbors) =
      VALUE tt_grid(
        FOR cell IN grid
          WHERE ( row >= row - 1
            AND row <= row + 1
            AND col >= col - 1
            AND col <= col + 1 )
            ( cell ) ).

    r_sitfra_2017 = me.
  ENDMETHOD.


  METHOD secondary_indices_are_cool.
    TYPES:
      BEGIN OF ty_cell,
        row   TYPE i,
        col   TYPE i,
        alive TYPE abap_bool,
      END OF ty_cell.

    TYPES tt_grid
      TYPE SORTED TABLE OF ty_cell
      WITH UNIQUE KEY row col
      WITH NON-UNIQUE SORTED KEY cell_state COMPONENTS alive.

    DATA(grid) =
      VALUE tt_grid(
        FOR i = 0 THEN i + 1 WHILE i < 5
        FOR j = 0 THEN j + 1 WHILE j < 5 (
            row = i
            col = j
            alive = abap_false
        ) ).


    grid[ row = 1 col = 2 ]-alive = abap_true.
    grid[ row = 2 col = 2 ]-alive = abap_true.
    grid[ row = 4 col = 3 ]-alive = abap_true.
    grid[ row = 4 col = 4 ]-alive = abap_true.

    DATA(alive_cell_count)
      = lines(
        FILTER #( grid USING KEY cell_state
          WHERE alive = abap_true ) ).

    r_sitfra_2017 = me.
  ENDMETHOD.

  METHOD but_there_are_some_caveats.
    TYPES:
      BEGIN OF ty_cell,
        row   TYPE i,
        col   TYPE i,
        alive TYPE abap_bool,
      END OF ty_cell.

    TYPES tt_grid TYPE SORTED TABLE OF ty_cell WITH UNIQUE KEY row col.

    DATA(grid) =
      VALUE tt_grid(
        FOR i = 0 THEN i + 1 WHILE i < 5
        FOR j = 0 THEN j + 1 WHILE j < 5 (
            row = i
            col = j
            alive = abap_false
        ) ).

    DATA(row) = 2.
    DATA(col) = 2.
    DATA(neighbors) =
      VALUE tt_grid(
        FOR cell IN grid
          WHERE ( row >= row - 1
            AND row <= row + 1
            AND col >= col - 1
            AND col <= col + 1 )
            ( cell ) ).

  ENDMETHOD.

ENDCLASS.
