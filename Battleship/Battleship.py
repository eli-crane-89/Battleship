from random import randint
import copy
n = []
#print len(n)
lst_ships = [1,2,3,4]
length_board = 8


def board_nums(length_board):
    board_nums = []
    for i in range(length_board + 1):
        if i != 0:
            board_nums.append(i)

    board_nums_char = []

    for i in range(length_board):
        board_nums_char.append(str(board_nums[i]))

    return board_nums_char


def ship_diction(lst_ships):
    ship_dict = {}

    if 1 in lst_ships:
        ship_dict['Tug Boat'] = 2
    if 2 in lst_ships:
        ship_dict['Cruiser'] = 3
    if 3 in lst_ships:
        ship_dict['Battleship'] = 4
    if 4 in lst_ships:
        ship_dict['Aircraft Carrier'] = 5

    return ship_dict

def ship_nomenclature(lst_ships):
    ship_names = []

    if 1 in lst_ships:
        ship_names.append("Tug Boat")
    if 2 in lst_ships:
        ship_names.append("Cruiser")
    if 3 in lst_ships:
        ship_names.append("Battleship")
    if 4 in lst_ships:
        ship_names.append("Aircraft Carrier")

    return ship_names


def remove_ships(n,lst_ships):
    if len(n) > 0:
        if n[2] == 1:
            lst_ships.remove(1)
        elif n[2] == 2:
            lst_ships.remove(2)
        elif n[2] == 3:
            lst_ships.remove(3)
        elif n[2] == 4:
            lst_ships.remove(4)
    return lst_ships


def make_board(size):
    board = []

    for x in range(size):
        board.append(["O"] * size)

    return board


def print_board(board):
    print
    for row in board:

        print " ".join(row)
    print


def ship_select(ship_sel):
    return ship_sel


def ship_select_text(ship_names):
    print
    if len(ship_names) == 4:
        str_raw = "Which ship you you like to place? You may choose %s, %s, %s or %s: " % (ship_names[0], ship_names[1], ship_names[2], ship_names[3])
    elif len(ship_names) == 3:
        str_raw = "Which ship you you like to place? You may choose %s, %s or %s: " % (ship_names[0], ship_names[1], ship_names[2])
    elif len(ship_names) == 2:
        str_raw = "Which ship you you like to place? You may choose %s or %s: " % (ship_names[0], ship_names[1])
    elif len(ship_names) == 1:
        str_raw = "Place your %s." % (ship_names[0])
    print

    i = 0
    if len(ship_names) > 1:
        while i != 1:
            ship_sel = raw_input(str_raw)
            if ship_sel in ship_names:
                print "You have selected to place your %s." % (ship_sel)
                i = 1
            else:
                print "That is not a valid ship, please try again."
    else:
        ship_sel = ship_names[0]
        print str_raw
    print

    return ship_sel


def ship_coord(ship_sel, board, ship_dict, board_nums):
        i = 0
        lst_coord = [0,0,0,0]
        lst_coord[2] = ship_dict[ship_sel] - 1


        while i != 1:
            t = 0
            while t != 1:
                ship_row = raw_input("Select Row: ")
                if ship_row in board_nums:
                    t = 1
                    tt = 0
                    while tt != 1:
                        ship_col = raw_input("Select Col: ")
                        if ship_col in board_nums:
                            tt = 1
                        else:
                            print
                            print "That is not a valid selection, use a number between %s and %s." % (board_nums[0], board_nums[len(board_nums) - 1])
                            print
                else:
                    print
                    print "That is not a valid selection, use a number between %s and %s." % (board_nums[0], board_nums[len(board_nums) - 1])
                    print


            ship_row = int(ship_row) - 1
            ship_col = int(ship_col) - 1


            lst_coord[0] = ship_row
            lst_coord[1] = ship_col


            num_dir = 0
            if lst_coord[0] - lst_coord[2] >= 0 and check_place(lst_coord,1,board) == 0:
                num_dir = num_dir + 1
            if lst_coord[0] + lst_coord[2] <= len(board) - 1 and check_place(lst_coord,2,board) == 0:
                num_dir = num_dir + 1
            if lst_coord[1] - lst_coord[2] >= 0 and check_place(lst_coord,3,board) == 0:
                num_dir = num_dir + 1
            if lst_coord[1] + lst_coord[2] <= len(board) - 1 and check_place(lst_coord,4,board) == 0:
                num_dir = num_dir + 1

            i = 0
            print
            if board[ship_row][ship_col] != "O":
                print "There is already a ship there. Select again."
                print_board(board)
            elif num_dir == 0:
                print "There are no valid directions for you to move. Select again."
                print_board(board)
            else:
                i = 1
            print

        return lst_coord


def place_ship(lc,board):
    i = 0
    str_dir = []
    if lc[0] - lc[2] >= 0 and check_place(lc,1,board) == 0:
        str_dir.append("up")
    if lc[0] + lc[2] <= len(board) - 1 and check_place(lc,2,board) == 0:
        str_dir.append("down")
    if lc[1] - lc[2] >= 0 and check_place(lc,3,board) == 0:
        str_dir.append("left")
    if lc[1] + lc[2] <= len(board) - 1 and check_place(lc,4,board) == 0:
        str_dir.append("right")

    return str_dir

def place_ship_text(str_dir,lc):
    if len(str_dir) == 4:
        print
        print "Please define the direction you would like to place your ship. You may choose %s, %s, %s or %s: " % (str_dir[0], str_dir[1], str_dir[2], str_dir[3])
        print
    elif len(str_dir) == 3:
        print
        print "Please define the direction you would like to place your ship. You may choose %s, %s or %s: " % (str_dir[0], str_dir[1], str_dir[2])
        print
    elif len(str_dir) == 2:
        print
        print "Please define the direction you would like to place your ship. You may choose %s or %s: " % (str_dir[0], str_dir[1])
        print
    elif len(str_dir) == 1:
        print
        print "Please define the direction you would like to place your ship. You may choose %s: " % (str_dir[0])
        print

    i = 0
    while i != 1:
        ship_dir = raw_input("Select Direction: ")
        if ship_dir.lower() in str_dir:
            print "You have chosen %s." % (ship_dir)
            if ship_dir.lower() == "up":
                lc[3] = 1
            elif ship_dir.lower() == "down":
                lc[3] = 2
            elif ship_dir.lower() == "left":
                lc[3] = 3
            elif ship_dir.lower() == "right":
                lc[3] = 4
            i = 1
        else:
            print
            print "That is not a valid selection. Try again."
            print

    return lc

def set_ship(lc, board):
    k = 0
    if lc[2] == 1:
        ship_lett = "T"
    elif lc[2] == 2:
        ship_lett = "C"
    elif lc[2] == 3:
        ship_lett = "B"
    else:
        ship_lett = "A"

    board[lc[0]][lc[1]] = ship_lett

    for i in range(lc[2]):
        k = k + 1
        if lc[3] == 1:
            board[lc[0] - k][lc[1]] = ship_lett
        elif lc[3] == 2:
            board[lc[0] + k][lc[1]] = ship_lett
        elif lc[3] == 3:
            board[lc[0]][lc[1] - k] = ship_lett
        else:
            board[lc[0]][lc[1] + k] = ship_lett

    return board


def check_place(lc,a,board):
    chk = 0
    i = 1
    hit_miss = ["H", "M"]

    for i in range(lc[2]):
        k = i + 1
        ship_letts = ["T","C", "B", "A"]

        if a == 1 and board[lc[0] - k][lc[1]] != "O":
            chk = chk + 1
            break
        elif a == 2 and board[lc[0] + k][lc[1]] != "O":
            chk = chk + 1
            break
        elif a == 3 and board[lc[0]][lc[1] - k] != "O":
            chk = chk + 1
            break
        elif a == 4 and board[lc[0]][lc[1] + k] != "O":
            chk = chk + 1
            break

    return chk

def ship_select_comp(lst_ship_names):

    if len(lst_ship_names) > 1:
        lst_num = randint(0, len(lst_ship_names) - 1)
    else:
        lst_num = 0

    ship_sel = lst_ship_names[lst_num]


    return ship_sel

def ship_coord_comp(ship_dict, ship_sel, board):
    i = 0
    lc = [0,0,0,0]
    lc[2] = ship_dict[ship_sel] - 1

    while i != 1:
        dir_list = []
        lc[0] = randint(0, len(board) - 1)
        lc[1] = randint(0, len(board) - 1)

        num_dir = 0

        if lc[0] - lc[2] >= 0 and check_place(lc,1,board) == 0:
            num_dir = num_dir + 1
            dir_list.append(1)
        if lc[0] + lc[2] <= len(board) - 1 and check_place(lc,2,board) == 0:
            num_dir = num_dir + 1
            dir_list.append(2)
        if lc[1] - lc[2] >= 0 and check_place(lc,3,board) == 0:
            num_dir = num_dir + 1
            dir_list.append(3)
        if lc[1] + lc[2] <= len(board) - 1 and check_place(lc,4,board) == 0:
            num_dir = num_dir + 1
            dir_list.append(4)

        if board[lc[0]][lc[1]] != "O":
            i = 0
        elif num_dir == 0:
            i = 0
        else:
            i = 1

    if len(dir_list) > 1:
        dir_choice = randint(0, len(dir_list) - 1)
    else:
        dir_choice = 0

    lc[3] = dir_list[dir_choice]

    return lc

def turn_hum(comp_board_hum, board_nums):
    i = 0
    g_coord = [0,0]
    print "This is the computer's board:"
    print
    print_board(comp_board_hum)
    print
    print "Select where you would like to strike: "

    while i != 1:
        t = 0
        while t != 1:
            g_row = raw_input("Guess Row: ")
            if g_row in board_nums:
                t = 1
                tt = 0
                while tt != 1:
                    g_col = raw_input("Guess Col: ")
                    if g_col in board_nums:
                        tt = 1
                    else:
                        print
                        print "That is not a valid selection. Please select a number between %s and %s." % (board_nums[0], board_nums[len(board_nums) - 1])
                        print
            else:
                print
                print "That is not a valid selection. Please select a number between %s and %s." % (board_nums[0], board_nums[len(board_nums) - 1])
                print


        g_row = int(g_row) - 1
        g_col = int(g_col) - 1

        if comp_board_hum[g_row][g_col] == "O":
            g_coord[0] = g_row
            g_coord[1] = g_col
            i = 1
        else:
            print
            print "You already went there, select again:"
            print

    return g_coord

def change_player_board(player_board, oppon_board, n_coord, ship_health, player_no):
    g_coord = [0,0]
    if player_no == 2:
        if n_coord[12] == 1:
            g_coord[0] = n_coord[13]
            g_coord[1] = n_coord[14]
        else:
            g_coord[0] = n_coord[0]
            g_coord[1] = n_coord[1]
    else:
        g_coord[0] = n_coord[0]
        g_coord[1] = n_coord[1]

    if oppon_board[g_coord[0]][g_coord[1]] != "O":
        strike_action(ship_health, player_no)
        player_board[g_coord[0]][g_coord[1]] = "H"
    else:
        player_board[g_coord[0]][g_coord[1]] = "M"
        if player_no == 1:
            print
            print "You missed."
            #print
        else:
            #print
            print "He missed."
            print

    return player_board

def change_ncomp(n_comp, ship_health, atckcoord):
    lost_ships = 0

    for i in range(0, len(ship_health)):
        if ship_health[i] == -1:
            lost_ships = lost_ships + 1

    if lost_ships > n_comp[15]:
        for i in range (15):
            n_comp[i] = 0
            n_comp[15] = lost_ships
    elif len(atckcoord) > 0 and n_comp[5] == 3:
        n_comp[0] = atckcoord[len(atckcoord) - 2]
        n_comp[1] = atckcoord[len(atckcoord) - 1]
        n_comp[5] = 0

    return n_comp

def change_oppon_board(oppon_board, n_coord, player_no):
    g_coord = [0,0]
    if player_no == 2:
        if n_coord[12] == 1:
            g_coord[0] = n_coord[13]
            g_coord[1] = n_coord[14]
        else:
            g_coord[0] = n_coord[0]
            g_coord[1] = n_coord[1]
    else:
        g_coord[0] = n_coord[0]
        g_coord[1] = n_coord[1]

    if oppon_board[g_coord[0]][g_coord[1]] == "O":
        oppon_board[g_coord[0]][g_coord[1]] = "M"
    else:
        oppon_board[g_coord[0]][g_coord[1]] = "H"

    return oppon_board

def ship_life(ship_health, oppon_board, n_coord, player_no):
    g_coord = [0,0]
    if player_no == 2:
        if n_coord[12] == 1:
            g_coord[0] = n_coord[13]
            g_coord[1] = n_coord[14]
        else:
            g_coord[0] = n_coord[0]
            g_coord[1] = n_coord[1]
    else:
        g_coord[0] = n_coord[0]
        g_coord[1] = n_coord[1]

    if oppon_board[g_coord[0]][g_coord[1]] == "T":
        ship_health[0] = ship_health[0] - 1
    elif oppon_board[g_coord[0]][g_coord[1]] == "C":
        ship_health[1] = ship_health[1] - 1
    elif oppon_board[g_coord[0]][g_coord[1]] == "B":
        ship_health[2] = ship_health[2] - 1
    elif oppon_board[g_coord[0]][g_coord[1]] == "A":
        ship_health[3] = ship_health[3] - 1

    return ship_health

def lower_ship_health(ship_health):
    if ship_health[0] == 0:
        ship_health[0] = -1
    elif ship_health[1] == 0:
        ship_health[1] = -1
    elif ship_health[2] == 0:
        ship_health[2] = -1
    elif ship_health[3] == 0:
        ship_health[3] = -1

    return ship_health


def strike_action(ship_health,player_no):
    first_pro = ""
    sec_pro = ""
    if player_no == 1:
        first_pro = "You"
        sec_pro = "his"
    else:
        first_pro = "He"
        sec_pro = "your"

    strike_string = ""
    if ship_health[0] == 0:
        strike_string = "%s sunk %s Tug Boat!" % (first_pro, sec_pro)
    elif ship_health[1] == 0:
        strike_string = "%s sunk %s Cruiser!" % (first_pro, sec_pro)
    elif ship_health[2] == 0:
        strike_string = "%s sunk %s Battleship!" % (first_pro, sec_pro)
    elif ship_health[3] == 0:
        strike_string = "%s sunk %s Aircraft Carrier!" % (first_pro, sec_pro)
    else:
        strike_string = "%s hit!" % (first_pro)

    print
    print strike_string
    print

def end_of_game(ship_health, player_no):
    #print 510
    #print ship_health[0] + ship_health[1] + ship_health[2] + ship_health[3]
    if ship_health[0] + ship_health[1] + ship_health[2] + ship_health[3] == -4:
        if player_no == 1:
            print
            print "Congratulations Commander, you win!"
            print
            return 1
        elif player_no == 2:
            print "Sorry, you lost."
            print
            return 2
        else:
            return 0

def comp_attack(n_comp, oppon_board, z, atck_coord):
    dirs = []
    hit_miss = ["H", "M"]
    n_comp[12] = 2

    #n_comp[4] = Search and destroy mode engaged(0,1)
    #if n_comp[4] > 0 and n_comp[5] == 0:
    while n_comp[12] == 2:
        n_comp[12] = 0
        if n_comp[4] > 0 and n_comp[7] != 2:
            #print 475
            #n_comp[8] - n_comp[11] refer to directions, n_comp[7] refer to if a good direction was found
            if n_comp[7] == 0:
                if oppon_board[n_comp[0]][n_comp[1]] == "H":
                    n_comp[7] = 1
                    if n_comp[0] > 0 and oppon_board[n_comp[0] - 1][n_comp[1]] not in hit_miss:
                        dirs.append(1)
                        n_comp[8] = 1
                    if n_comp[0] < len(oppon_board) - 1 and oppon_board[n_comp[0]+ 1][n_comp[1]] not in hit_miss:
                        dirs.append(2)
                        n_comp[9] = 2
                    if n_comp[1] > 0 and oppon_board[n_comp[0]][n_comp[1] - 1] not in hit_miss:
                        dirs.append(3)
                        n_comp[10] = 3
                    if n_comp[1] < len(oppon_board) - 1 and oppon_board[n_comp[0]][n_comp[1] + 1] not in hit_miss:
                        dirs.append(4)
                        n_comp[11] = 4
            else:
                if n_comp[8] == 1:
                    dirs.append(1)
                if n_comp[9] == 2:
                    dirs.append(2)
                if n_comp[10] == 3:
                    dirs.append(3)
                if n_comp[11] == 4:
                    dirs.append(4)

            #print 622
            #print dirs
            rnd_dir = randint(0, len(dirs) - 1)
            n_comp[3] = dirs[rnd_dir]
            #print 55734
            #print n_comp


            if n_comp[3] == 1:
                if oppon_board[n_comp[0] - 1][n_comp[1]] != "O":
                    #print 564
                    if n_comp[0] == len(oppon_board) - 1:
                        n_comp[5] = n_comp[5] + 1
                    n_comp[0] = n_comp[0] - 1
                    n_comp[6] = n_comp[6] + 1
                    n_comp[5] = n_comp[5] + 1
                    n_comp[7] = n_comp[7] + 1
                else:
                    #print 570
                    n_comp[12] = 1
                    n_comp[13] = n_comp[0] - 1
                    n_comp[14] = n_comp[1]
                    n_comp[7 + n_comp[3]] = 0
            elif n_comp[3] == 2:
                if oppon_board[n_comp[0] + 1][n_comp[1]] != "O":
                    #print 575
                    if n_comp[0] == 0:
                        n_comp[5] = n_comp[5] + 1
                    n_comp[0] = n_comp[0] + 1
                    n_comp[6] = n_comp[6] + 1
                    n_comp[5] = n_comp[5] + 1
                    n_comp[7] = n_comp[7] + 1
                else:
                    #print 581
                    n_comp[12] = 1
                    n_comp[13] = n_comp[0] + 1
                    n_comp[14] = n_comp[1]
                    n_comp[7 + n_comp[3]] = 0
            elif n_comp[3] == 3:
                if oppon_board[n_comp[0]][n_comp[1] - 1] != "O":
                    #print 586
                    if n_comp[1] == len(oppon_board) - 1:
                        n_comp[5] = n_comp[5] + 1
                    n_comp[1] = n_comp[1] - 1
                    n_comp[6] = n_comp[6] + 1
                    n_comp[5] = n_comp[5] + 1
                    n_comp[7] = n_comp[7] + 1
                else:
                    #print 592
                    n_comp[12] = 1
                    n_comp[13] = n_comp[0]
                    n_comp[14] = n_comp[1] - 1
                    n_comp[7 + n_comp[3]] = 0
            elif n_comp[3] == 4:
                if oppon_board[n_comp[0]][n_comp[1] + 1] != "O":
                    #print 602
                    if n_comp[1] == 0:
                        n_comp[5] = n_comp[5] + 1
                    n_comp[1] = n_comp[1] + 1
                    n_comp[6] = n_comp[6] + 1
                    n_comp[5] = n_comp[5] + 1
                    n_comp[7] = n_comp[7] + 1
                else:
                    #print 609
                    n_comp[12] = 1
                    n_comp[13] = n_comp[0]
                    n_comp[14] = n_comp[1] + 1
                    n_comp[7 + n_comp[3]] = 0

        #n_comp[5] = Streak indicator, n_comp[6] = switch direction multiplier
        elif oppon_board[n_comp[0]][n_comp[1]] != "O" and n_comp[4] > 0 and n_comp[5] == 1 and oppon_board[n_comp[0]][n_comp[1]] != "M":
            #print 506
            if n_comp[3] == 1 and n_comp[0] > 0 and oppon_board[n_comp[0] - 1][n_comp[1]] not in hit_miss:
                n_comp[0] = n_comp[0] - 1
                n_comp[6] = n_comp[6] + 1
            elif n_comp[3] == 1 and oppon_board[n_comp[0] + 1 + n_comp[6]][n_comp[1]] not in hit_miss:
                n_comp[0] = n_comp[0] + 1 + n_comp[6]
                n_comp[3] = 2
                n_comp[5] = n_comp[5] + 1
            elif n_comp[3] == 2 and n_comp[0] < len(oppon_board) - 1 and oppon_board[n_comp[0] + 1][n_comp[1]] not in hit_miss:
                n_comp[0] = n_comp[0] + 1
                n_comp[6] = n_comp[6] + 1
            elif n_comp[3] == 2  and oppon_board[n_comp[0] - 1 - n_comp[6]][n_comp[1]] not in hit_miss:
                n_comp[0] = n_comp[0] - 1 - n_comp[6]
                n_comp[3] = 1
                n_comp[5] = n_comp[5] + 1
            elif n_comp[3] == 3 and n_comp[1] > 0 and oppon_board[n_comp[0]][n_comp[1] - 1] not in hit_miss:
                n_comp[1] = n_comp[1] - 1
                n_comp[6] = n_comp[6] + 1
            elif n_comp[3] == 3 and oppon_board[n_comp[0]][n_comp[1] + 1 + n_comp[6]] not in hit_miss:
                n_comp[1] = n_comp[1] + 1 + n_comp[6]
                n_comp[3] = 4
                n_comp[5] = n_comp[5] + 1
            elif n_comp[3] == 4 and n_comp[1] < len(oppon_board) - 1 and oppon_board[n_comp[0]][n_comp[1] + 1] not in hit_miss:
                n_comp[1] = n_comp[1] + 1
                n_comp[6] = n_comp[6] + 1
            elif n_comp[3] == 4 and oppon_board[n_comp[0]][n_comp[1] - 1 - n_comp[6]] not in hit_miss:
                n_comp[1] = n_comp[1] - 1 - n_comp[6]
                n_comp[3] = 3
                n_comp[5] = n_comp[5] + 1
            else:
                #print 658
                n_comp = comp_roll(n_comp, oppon_board, atck_coord)

        elif n_comp[4] > 0 and n_comp[5] == 1 and oppon_board[n_comp[0]][n_comp[1]] == "M":
            #print 633
            n_comp[5] = n_comp[5] + 1
            if n_comp[3] == 1 and oppon_board[n_comp[0] + n_comp[4] + 1][n_comp[1]] not in hit_miss:
                #print "z1"
                n_comp[0] = n_comp[0] + n_comp[4] + 1
                n_comp[3] = 2
            elif n_comp[3] == 1:
                #print "z2"
                n_comp = comp_roll(n_comp, oppon_board, atck_coord)
            elif n_comp[3] == 2 and oppon_board[n_comp[0] - n_comp[4] - 1][n_comp[1]] not in hit_miss:
                #print "z3"
                n_comp[0] = n_comp[0] - n_comp[4] - 1
                n_comp[3] = 1
            elif n_comp[3] == 2:
                #print "z4"
                n_comp = comp_roll(n_comp, oppon_board, atck_coord)
            elif n_comp[3] == 3 and oppon_board[n_comp[0]][n_comp[1] + n_comp[4] + 1] not in hit_miss:
                #print "z5"
                n_comp[1] = n_comp[1] + n_comp[4] + 1
                n_comp[3] = 4
            elif n_comp[3] == 3:
                #print "z6"
                n_comp = comp_roll(n_comp, oppon_board, atck_coord)
            elif n_comp[3] == 4 and oppon_board[n_comp[0]][n_comp[1] - n_comp[4] - 1] not in hit_miss:
                #print "z7"
                n_comp[1] = n_comp[1] - n_comp[4] - 1
                n_comp[3] = 3
            else:
                #print "z8"
                n_comp = comp_roll(n_comp, oppon_board, atck_coord)

        elif n_comp[5] == 2 and oppon_board[n_comp[0]][n_comp[1]] == "M":
            #print 632
            if n_comp[3] == 1 and oppon_board[n_comp[0] + n_comp[6]][n_comp[1]] not in hit_miss:
                n_comp[0] = n_comp[0] + n_comp[6]
                n_comp[3] = 2
                n_comp[5] = n_comp[5] + 1
            elif n_comp[3] == 2 and oppon_board[n_comp[0] - n_comp[6]][n_comp[1]] not in hit_miss:
                n_comp[0] = n_comp[0] - n_comp[6]
                n_comp[3] = 1
                n_comp[5] = n_comp[5] + 1
            elif n_comp[3] == 3 and oppon_board[n_comp[0]][n_comp[1] + n_comp[6]] not in hit_miss:
                n_comp[0] = n_comp[1] + n_comp[6]
                n_comp[3] = 4
                n_comp[5] = n_comp[5] + 1
            elif oppon_board[n_comp[0]][n_comp[1] - n_comp[6]] not in hit_miss:
                n_comp[0] = n_comp[1] - n_comp[6]
                n_comp[3] = 3
                n_comp[5] = n_comp[5] + 1
            else:
                n_comp = comp_roll(n_comp, oppon_board, atck_coord)


        elif n_comp[5] == 2 and oppon_board[n_comp[0]][n_comp[1]] != "M":
            #print 651
            if n_comp[3] == 1:
                if oppon_board[n_comp[0] - 1][n_comp[1]] not in hit_miss:
                    n_comp[0] = n_comp[0] - 1
                else:
                    n_comp[5] = 3
                    comp_roll(n_comp, oppon_board)
            elif n_comp[3] == 2:
                if oppon_board[n_comp[0] + 1][n_comp[1]] not in hit_miss:
                    n_comp[0] = n_comp[0] + 1
                else:
                    n_comp[5] = 3
                    comp_roll(n_comp, oppon_board)
            elif n_comp[3] == 3:
                if oppon_board[n_comp[0]][n_comp[1] - 1] not in hit_miss:
                    n_comp[1] = n_comp[1] - 1
                else:
                    n_comp[5] = 3
                    comp_roll(n_comp, oppon_board)
            else:
                if oppon_board[n_comp[0]][n_comp[1] + 1] not in hit_miss:
                    n_comp[1] = n_comp[1] + 1
                else:
                    n_comp[5] = 3
                    comp_roll(n_comp, oppon_board)
        else:
            #print 734
            n_comp = comp_roll(n_comp, oppon_board, atck_coord)

    #print 718
    #print n_comp
    #print_board(oppon_board)
    #print oppon_board[n_comp[0]][n_comp[1]]
    #print oppon_board[n_comp[13]][n_comp[14]]
    #print n_comp[4]

    if n_comp[12] <> 1:
        if oppon_board[n_comp[0]][n_comp[1]] == "O":
            n_comp[12] = 1
            n_comp[13] = n_comp[0]
            n_comp[14] = n_comp[1]
        elif n_comp[12] <> 1:
            n_comp[4] = n_comp[4] + 1

    return n_comp

def comp_roll(n_comp, oppon_board, atck_coord):
    #print 533

    if len(atck_coord) > 0 and n_comp[4] == 0:
        #print "527w"
        n_comp[0] = atck_coord[len(atck_coord) - 2]
        n_comp[1] = atck_coord[len(atck_coord) - 1]
        for i in range(2,15):
            n_comp[i] = 0
        n_comp[4] = 1
        n_comp[3] = 0
        n_comp[12] = 2

    else:
        #print "784t"
        for i in range(15):
            n_comp[i] = 0

        if 1 == 1:
            i = 0
            while i != 1:
                g_row = randint(0, len(oppon_board) - 1)
                g_col = randint(0, len(oppon_board) - 1)

                if oppon_board[g_row][g_col] != "M" and oppon_board[g_row][g_col] != "H":
                    n_comp[0] = g_row
                    n_comp[1] = g_col
                    i = 1
                else:
                    i = 0

            if oppon_board[n_comp[0]][n_comp[1]] == "O" and n_comp[4] != 1:
                #print 768
                n_comp[12] = 1
                n_comp[13] = n_comp[0]
                n_comp[14] = n_comp[1]

    return n_comp

def attack_coords(atck_coord, n_comp, oppon_board, ship_health_hum, shiphealth_lturn, hum_board_set):
    ship_dest = 0
    k = 1
    x = 0

    if n_comp[12] != 1:
        atck_coord.append(n_comp[0])
        atck_coord.append(n_comp[1])

    for i in range(len(ship_health_hum)):
        if ship_health_hum[i] == -1 and shiphealth_lturn[i] != ship_health_hum[i]:
            ship_dest = k
        k = k + 1

    if ship_dest > 0:
        if ship_dest == 1:
            ship_lett = "T"
        elif ship_dest == 2:
            ship_lett = "C"
        elif ship_dest == 3:
            ship_lett = "B"
        else:
            ship_lett = "A"

        for i in range(len(oppon_board)):
            for j in range(len(oppon_board)):
                if hum_board_set[i][j] == ship_lett:
                    x = 0
                    #print atck_coord

                    while x < len(atck_coord)/2:
                        if atck_coord[x * 2] == i and atck_coord[x * 2 + 1] == j:
                            atck_coord.pop(x * 2 + 1)
                            atck_coord.pop(x * 2)
                        x = x + 1

    return atck_coord


############# Test Code #############
print "Let's play Battleship!"
print
hum_board_hum = make_board(length_board)
comp_board_comp = make_board(length_board)
hum_board_comp = make_board(length_board)
comp_board_hum = make_board(length_board)
print_board(hum_board_hum)
n_comb = [0,0,0,0,0,0,0,0,0,0,0,0]
n_comp = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
attack_coord = []
ship_health_hum_lastturn = [2,3,4,5]
hum_board_set = []


#############THIS IS WHERE YOU PLACE THE SHIPS#############

for i in range(4):
    ship_dict = ship_diction(lst_ships)
    ship_names = ship_nomenclature(lst_ships)
    ship_sel = ship_select(ship_select_text(ship_names))
    n_hum = ship_coord(ship_sel, hum_board_hum, ship_dict, board_nums(length_board))
    hum_board_hum = set_ship(place_ship_text(place_ship(n_hum, hum_board_hum),n_hum), hum_board_hum)
    human_board_comp = hum_board_hum
    lst_ships = remove_ships(n_hum, lst_ships)
    print_board(hum_board_hum)

"""
for i in range(4):
    ship_dict = ship_diction(lst_ships)
    ship_names = ship_nomenclature(lst_ships)
    n_hum = ship_coord_comp(ship_dict, ship_select_comp(ship_names), hum_board_hum)
    hum_board_hum = set_ship(n_hum, hum_board_hum)
    lst_ships = remove_ships(n_hum, lst_ships)
"""

print_board(hum_board_hum)
hum_board_set = copy.deepcopy(hum_board_hum)
lst_ships = [1,2,3,4]
ship_health_comp = [2,3,4,5]
ship_health_hum = [2,3,4,5]
z = 0

for i in range(4):
    ship_dict_comp = ship_diction(lst_ships)
    ship_names = ship_nomenclature(lst_ships)
    n_comb = ship_coord_comp(ship_dict_comp, ship_select_comp(ship_names), comp_board_comp)
    comp_board_comp = set_ship(n_comb, comp_board_comp)
    lst_ships = remove_ships(n_comb, lst_ships)

print_board(comp_board_comp)


#print 642
#print n_comp
#############THIS IS WHERE THE TURNS START#############

while z < 100:
    if z == 0:
	print "Let's play Battleship!"
	print

    player_no = 1
    n_hum = turn_hum(comp_board_hum, board_nums(length_board))
    ship_health_comp = ship_life(ship_health_comp, comp_board_comp, n_hum, player_no)
    comp_board_hum = change_player_board(comp_board_hum, comp_board_comp, n_hum, ship_health_comp, player_no)
    comp_board_comp = change_oppon_board(comp_board_comp, n_hum, player_no)
    ship_health_comp = lower_ship_health(ship_health_comp)
    end_game = end_of_game(ship_health_comp, player_no)

    if end_game > 0:
        break

    #print "This is the computer board that the human sees:"
    #print
    #print_board(comp_board_hum)

    #print "This is the board that the computer sees:"
    #print
    #print_board(comp_board_comp)


######COMPUTER TURN########

    player_no = 2
    #print 672
    #print n_comp
    print
    n_comp = comp_attack(n_comp, hum_board_hum, z, attack_coord)
    #print
    #print "This is the computer's moves:"
    #print n_comp
    print
    ship_health_hum_lastturn = lower_ship_health(ship_health_hum_lastturn)
    ship_health_hum = ship_life(ship_health_hum, hum_board_hum, n_comp, player_no)
    hum_board_comp = change_player_board(hum_board_comp, hum_board_hum, n_comp, ship_health_hum, player_no)
    ship_health_hum_lastturn = ship_life(ship_health_hum_lastturn, hum_board_hum, n_comp, player_no)
    hum_board_hum = change_oppon_board(hum_board_hum, n_comp, player_no)
    ship_health_hum = lower_ship_health(ship_health_hum)
    attack_coord = attack_coords(attack_coord,n_comp, hum_board_hum, ship_health_hum, ship_health_hum_lastturn, hum_board_set)

    n_comp = change_ncomp(n_comp, ship_health_hum, attack_coord)
    #print 815
    #print n_comp
    #print 818
    #print attack_coord
    end_game = end_of_game(ship_health_hum, player_no)

    if end_game > 0:
        break


   # print 853
    #print ship_health_hum
    print
    #print n
    #print "This is the human board that the comp sees:"
    #print
    #print_board(hum_board_comp)
    print
    print "This is your board:"
    print
    print_board(hum_board_hum)
    print

    z = z + 1

