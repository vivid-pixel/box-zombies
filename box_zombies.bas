'===== Box Zombies =====

' Require variables to be declared
OPTION _EXPLICIT

' Game config
CONST SCREEN_WIDTH = 640
CONST SCREEN_HEIGHT = 480
CONST COLLISION_BUFFER = 2

' Booleans
CONST FALSE = 0
CONST TRUE = NOT FALSE

' Colors
CONST EM_GREEN = _RGB32(44, 238, 111)
CONST HOT_PINK = _RGB32(222, 6, 127)

' Game objects
TYPE ENEMY
    AS _UNSIGNED LONG spr_color
    AS INTEGER alive, x_pos, y_pos, speed, spr_size
END TYPE

TYPE PLAYER
    AS _UNSIGNED LONG spr_color
    AS INTEGER alive, x_pos, y_pos, speed, spr_size
END TYPE

' Game object instances and other global attributes
DIM SHARED p1 AS PLAYER
DIM SHARED enemies(0 TO 19) AS ENEMY
DIM SHARED AS INTEGER center_x, center_y
center_x = SCREEN_WIDTH / 2
center_y = SCREEN_HEIGHT / 2

' Fire the missiles!
Game


SUB InitPlayer
    p1.alive = TRUE
    p1.speed = 2
    p1.spr_color = HOT_PINK
    p1.spr_size = 6
    p1.x_pos = center_x
    p1.y_pos = center_y
END SUB


SUB InitEnemiesArray
    DIM AS INTEGER id
    FOR id = LBOUND(enemies) TO UBOUND(enemies)
        enemies(id).alive = FALSE
        enemies(id).speed = 1
        enemies(id).spr_color = EM_GREEN
        enemies(id).spr_size = 6
        enemies(id).x_pos = 0
        enemies(id).y_pos = 0
    NEXT id
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


FUNCTION CheckCollision (id AS INTEGER)
    DIM AS INTEGER is_colliding
    is_colliding = (ABS(p1.x_pos - enemies(id).x_pos) + COLLISION_BUFFER <= p1.spr_size + _
                    enemies(id).spr_size AND ABS(p1.y_pos - enemies(id).y_pos) + _
                    COLLISION_BUFFER <= p1.spr_size + enemies(id).spr_size)

    ' Return
    IF is_colliding THEN CheckCollision = TRUE ELSE CheckCollision = FALSE
END FUNCTION


' Functions can't use the QB64 syntax for defining multiple variables of same type :(
FUNCTION CheckShot (shot_x%, shot_y%, enemy AS ENEMY)
    DIM AS INTEGER enemy_hit

    enemy_hit = (ABS(shot_x% - enemy.x_pos%) + ABS(shot_y% - enemy.y_pos%) <= _
                    enemy.spr_size + COLLISION_BUFFER * 2)

    ' Return
    CheckShot = enemy_hit
END FUNCTION


SUB Game
    ' Do some prep work
    SCREEN _NEWIMAGE(SCREEN_WIDTH, SCREEN_HEIGHT, 32)
    RANDOMIZE TIMER
    InitEnemiesArray
    InitPlayer

    ' Friendly names for movement keys
    DIM AS INTEGER move_up, move_down, move_left, move_right

    ' Friendly names for cursor attributes
    DIM AS INTEGER cursor_x, cursor_y, left_click, left_click_held ', middle_click, right_click

    ' Timers and their handlers
    DIM AS INTEGER spawn_timer
    spawn_timer = _FREETIMER
    ON TIMER(spawn_timer, 5) SpawnEnemy
    TIMER(spawn_timer) ON

    ' Game loop
    DO
        CLS ' Erase all the things
        _LIMIT 60 ' Frames per second

        ' Define movement keys
        move_up = _KEYDOWN(119) ' W
        move_down = _KEYDOWN(115) ' S
        move_left = _KEYDOWN(97) ' A
        move_right = _KEYDOWN(100) ' D

        ' Handle movement with keyboard inputs
        IF move_up THEN p1.y_pos = p1.y_pos - p1.speed
        IF move_down THEN p1.y_pos = p1.y_pos + p1.speed
        IF move_left THEN p1.x_pos = p1.x_pos - p1.speed
        IF move_right THEN p1.x_pos = p1.x_pos + p1.speed

        ' Draw the player "sprite"
        LINE (p1.x_pos, p1.y_pos)-(p1.x_pos + p1.spr_size,_
                    p1.y_pos + p1.spr_size), p1.spr_color, BF

        ' Poll mouse input
        WHILE _MOUSEINPUT: WEND
        cursor_x = _MOUSEX
        cursor_y = _MOUSEY
        left_click = _MOUSEBUTTON(1)
        'middle_click = _MOUSEBUTTON(3)
        'right_cick = _MOUSEBUTTON(2)

        DIM AS INTEGER id ', is_colliding
        ' Draw enemy sprites and update their x,y coordinates
        FOR id = LBOUND(enemies) TO UBOUND(enemies)
            IF enemies(id).alive THEN
                ' Check for collision between player and enemy sprite
                IF CheckCollision(id) THEN
                    SOUND 37, 4.5, 0.25, 0.0, 2 ' Simulate damage with a sound
                END IF

                ' Render enemy.
                CIRCLE (enemies(id).x_pos, enemies(id).y_pos), enemies(id).spr_size, _
                        enemies(id).spr_color

                ' Enemy movement is less predictable
                IF INT(RND * 6) + 1 = 1 THEN
                    ' Enemies follow the player -- no fancy formula needed
                    IF p1.y_pos < enemies(id).y_pos THEN
                        enemies(id).y_pos = enemies(id).y_pos - enemies(id).speed
                    END IF
                    IF p1.y_pos > enemies(id).y_pos THEN
                        enemies(id).y_pos = enemies(id).y_pos + enemies(id).speed
                    END IF
                    IF p1.x_pos < enemies(id).x_pos THEN
                        enemies(id).x_pos = enemies(id).x_pos - enemies(id).speed
                    END IF
                    IF p1.x_pos > enemies(id).x_pos THEN
                        enemies(id).x_pos = enemies(id).x_pos + enemies(id).speed
                    END IF
                END IF
            ELSE
                IF enemies(id).x_pos THEN ' Enemy has a nonzero coordinate  -- it's a corpse
                    CIRCLE (enemies(id).x_pos, enemies(id).y_pos), enemies(id).spr_size, _
                            _RGB32(133, 0, 55)
                END IF
            END IF

            IF left_click AND NOT left_click_held THEN ' Has player taken a shot?
                left_click_held = TRUE ' Prevent player holding fire button

                IF CheckShot(cursor_x, cursor_y, enemies(id)) THEN
                    ' Player hits an enemy -- intentionally or otherwise
                    enemies(id).alive = FALSE
                END IF
            ELSE
                left_click_held = FALSE
            END IF

        NEXT id
        _DISPLAY
    LOOP UNTIL _KEYDOWN(27) ' Escape key to exit
    SYSTEM ' Instantly quits game
END SUB
