' Box Zombies -- box_zombies.bas


' Require variables to be declared
OPTION _EXPLICIT

' Game config
CONST SCREEN_WIDTH = 640
CONST SCREEN_HEIGHT = 480
CONST HIT_BUFFER = 4

' Booleans
CONST FALSE = 0
CONST TRUE = NOT FALSE

' Colors
CONST EM_GREEN = _RGB32(44, 238, 111)
CONST HOT_PINK = _RGB32(222, 6, 127)

' Game objects
TYPE ENEMY
    AS _UNSIGNED LONG spr_color
    AS INTEGER alive, speed, spr_size, x_pos, y_pos
END TYPE

TYPE PLAYER
    AS _UNSIGNED LONG spr_color
    AS INTEGER alive, health, score, speed, spr_size, x_pos, y_pos
END TYPE

' Game object instances and other global attributes
DIM SHARED p1 AS PLAYER
DIM SHARED enemies(0 TO 19) AS ENEMY
DIM SHARED AS LONG center_x, center_y
center_x = SCREEN_WIDTH \ 2
center_y = SCREEN_HEIGHT \ 2

' Entry point
GameStart


' |||||||||| Functions Begin ||||||||||||
FUNCTION CheckCollision (id%)
    DIM AS INTEGER is_colliding
    is_colliding = (ABS(p1.x_pos - enemies(id%).x_pos) <= p1.spr_size - HIT_BUFFER +_
                    enemies(id%).spr_size AND ABS(p1.y_pos - enemies(id%).y_pos)_
                    <= p1.spr_size - HIT_BUFFER + enemies(id%).spr_size)

    IF is_colliding THEN CheckCollision = TRUE ELSE CheckCollision = FALSE
END FUNCTION


FUNCTION CheckShot (shot_x%, shot_y%, enemy AS ENEMY)
    DIM AS INTEGER enemy_hit
    enemy_hit = ABS(shot_x% - enemy.x_pos%) + ABS(shot_y% - enemy.y_pos%) <= _
                    enemy.spr_size + HIT_BUFFER

    CheckShot = enemy_hit
END FUNCTION
' |||||||||| Functions End ||||||||||||||


' |||||||||| Subroutines Begin ||||||||||
SUB DrawEnemy (id%)
    IF enemies(id%).alive THEN
        ' Render enemy as a box
                LINE (enemies(id%).x_pos, enemies(id%).y_pos)-(enemies(id%).x_pos +_
                        enemies(id%).spr_size, enemies(id%).y_pos + enemies(id%).spr_size),_
                        enemies(id%).spr_color, BF
    ELSE
        ' Check if enemy has a nonzero coordinate (meaning it is a corpse)
        IF enemies(id%).x_pos THEN
                    LINE (enemies(id%).x_pos, enemies(id%).y_pos)-(enemies(id%).x_pos +_
                        enemies(id%).spr_size, enemies(id%).y_pos + enemies(id%).spr_size),_
                        _RGB32(133, 0, 55), BF
        END IF
    END IF
END SUB


SUB DrawHUD
    DIM AS LONG lines, chars

    ' Calculate quantity of text columns & rows that fit within our screen
    lines = _HEIGHT \ 16
    chars = _WIDTH \ 8

    ' Transparent background for HUD so we don't block the view
    _PRINTMODE _KEEPBACKGROUND

    ' Draw to the lower-right corner
    LOCATE lines - 2, chars - 16
    PRINT "Health:"; p1.health
    LOCATE lines - 3, chars - 16
    PRINT "Score:"; p1.score
END SUB


SUB DrawPlayer
    ' Draw the player "sprite"
    CIRCLE (p1.x_pos, p1.y_pos), p1.spr_size, p1.spr_color
END SUB


SUB GameStart
    ' Do some prep work
    SCREEN _NEWIMAGE(SCREEN_WIDTH, SCREEN_HEIGHT, 32)
    RANDOMIZE TIMER
    InitEnemies
    InitPlayer

    ' Timers and their handlers
    DIM AS INTEGER spawn_timer
    spawn_timer = _FREETIMER
    ON TIMER(spawn_timer, 5) SpawnEnemy
    TIMER(spawn_timer) ON

    ' Friendly names for cursor attributes
    DIM AS INTEGER cursor_x, cursor_y, left_click, left_click_held ', middle_click, right_click

    ' Game loop
    DO
        CLS ' Erase all the things
        _LIMIT 60 ' Frames per second

        MovePlayer
        DrawPlayer

        ' Poll mouse input
        WHILE _MOUSEINPUT: WEND
        cursor_x = _MOUSEX
        cursor_y = _MOUSEY
        left_click = _MOUSEBUTTON(1)
        'middle_click = _MOUSEBUTTON(3)
        'right_cick = _MOUSEBUTTON(2)

        DIM AS INTEGER id

        ' Draw enemy sprites and update their x,y coordinates
        FOR id = LBOUND(enemies) TO UBOUND(enemies)
            MoveEnemy id
            DrawEnemy id

            ' Check for collision between player and enemy sprite
            IF CheckCollision(id) THEN
                ' Reduce player health
                p1.health = p1.health - 5
            END IF

            ' Has player taken a shot?
            IF left_click THEN
                ' Limit player to semi-automatic shots (one per click, can't hold button)
                left_click_held = TRUE

                IF CheckShot(cursor_x, cursor_y, enemies(id)) THEN
                    ' Player hits an enemy -- intentionally or otherwise
                    IF enemies(id).alive THEN
                        p1.score = p1.score + 10
                        enemies(id).alive = FALSE
                    END IF
                ELSE
                    ' Player no longer holding button
                    left_click_held = FALSE
                END IF
            END IF
        NEXT id

        DrawHUD ' Called last so the HUD stays on top

        _DISPLAY ' Update screen
    LOOP UNTIL _KEYDOWN(27) ' Escape key to exit
    SYSTEM ' Instantly quits game
END SUB


SUB InitEnemies
    DIM AS INTEGER id
    FOR id = LBOUND(enemies) TO UBOUND(enemies)
        enemies(id).alive = FALSE
        enemies(id).speed = 1
        enemies(id).spr_color = EM_GREEN
        enemies(id).spr_size = 8
        enemies(id).x_pos = 0
        enemies(id).y_pos = 0
    NEXT id
END SUB


SUB InitPlayer
    p1.alive = TRUE
    p1.health = 100
    p1.speed = 2
    p1.spr_color = HOT_PINK
    p1.spr_size = 6
    p1.x_pos = center_x
    p1.y_pos = center_y
END SUB


SUB MoveEnemy (id%)
    IF enemies(id%).alive THEN
        ' Randomize enemy movement to look more organic
        IF INT(RND * 6) + 1 = 1 THEN
            ' Enemies follow the player -- no fancy formula needed
            IF p1.y_pos < enemies(id%).y_pos THEN
                enemies(id%).y_pos = enemies(id%).y_pos - enemies(id%).speed
            END IF
            IF p1.y_pos > enemies(id%).y_pos THEN
                enemies(id%).y_pos = enemies(id%).y_pos + enemies(id%).speed
            END IF
            IF p1.x_pos < enemies(id%).x_pos THEN
                enemies(id%).x_pos = enemies(id%).x_pos - enemies(id%).speed
            END IF
            IF p1.x_pos > enemies(id%).x_pos THEN
                enemies(id%).x_pos = enemies(id%).x_pos + enemies(id%).speed
            END IF
        END IF
    END IF
END SUB


SUB MovePlayer
    ' Changes in player movement
    DIM AS INTEGER delta_x, delta_y

    ' Friendly names for movement keys
    DIM AS INTEGER move_up, move_down, move_left, move_right

    ' Define movement keys
    move_up = _KEYDOWN(119) ' W
    move_down = _KEYDOWN(115) ' S
    move_left = _KEYDOWN(97) ' A
    move_right = _KEYDOWN(100) ' D

    ' Handle movement with keyboard inputs
    IF move_up THEN delta_y = delta_y - 1
    IF move_down THEN delta_y = delta_y + 1
    IF move_left THEN delta_x = delta_x - 1
    IF move_right THEN delta_x = delta_x + 1

    ' Cap movement speed so diagonals aren't faster
    IF delta_x > 1 THEN
        delta_x = 1
    ELSE IF delta_x < -1 THEN delta_x = -1
    END IF

    IF delta_y > 1 THEN
        delta_y = 1
    ELSE IF delta_y < -1 THEN delta_y = -1
    END IF

    ' Now move the player
    p1.x_pos = p1.x_pos + delta_x
    p1.y_pos = p1.y_pos + delta_y
END SUB


SUB SpawnEnemy
    DIM AS INTEGER id, new_enemy

    ' Random enemy ID number from the first one (ID 0) to the last one in the array
    id = INT(RND * UBOUND(enemies))

    ' Has this enemy spawned before?
    new_enemy = NOT enemies(id).alive AND NOT enemies(id).x_pos

    IF NOT new_enemy THEN
        IF enemies(id).alive THEN ' Existing enemy earns speed increase of 25%
            enemies(id).speed = enemies(id).speed * 1.25
        ELSE ' Dead enemy, which respawns back to life at same position
            enemies(id).alive = TRUE
            enemies(id).spr_color = EM_GREEN
        END IF
    ELSE ' New enemy spawns at random coordinates.
        enemies(id).x_pos = INT(RND * SCREEN_WIDTH)
        enemies(id).y_pos = INT(RND * SCREEN_HEIGHT)
        enemies(id).alive = TRUE
    END IF
END SUB
' |||||||||| Subroutines End ||||||||||

