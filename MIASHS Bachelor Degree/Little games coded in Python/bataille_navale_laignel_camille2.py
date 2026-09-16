import os
import csv


def créer_grille_init():
    return [["~","~","~","~","~","~","~","~","~","~"],
            ["~","~","~","~","~","~","~","~","~","~"],
            ["~","~","~","~","~","~","~","~","~","~"],
            ["~","~","~","~","~","~","~","~","~","~"],
            ["~","~","~","~","~","~","~","~","~","~"],
            ["~","~","~","~","~","~","~","~","~","~"],
            ["~","~","~","~","~","~","~","~","~","~"],
            ["~","~","~","~","~","~","~","~","~","~"],
            ["~","~","~","~","~","~","~","~","~","~"],
            ["~","~","~","~","~","~","~","~","~","~"],]

def afficher_grille():
    lst = ["A","B","C","D","E","F","G","H","I","J"]
    print("        1       2       3       4       5       6       7       8       9      10")
    for i in range(10):
        print(lst[i], end="\t")
        for j in range(10):
            print(grille[i][j], end="\t")
        print(end="\n")

def charger_bateaux(nom):
    with open(nom,"rt", encoding="utf-8") as fic :
        dict = {}
        lecteur = csv.reader(fic, delimiter=";")
        for line in lecteur :
            dict[line[0]] = list(line[1:])
    return dict

def est_position_valide(chn):
    lettres_possibles = "ABCDEFGHIJ"
    chiffre_possible = ["1","2","3","4","5","6","7","8","9","10"]    
    if len(chn) != 2 and len(chn) != 3 :
        return False
    carlet, chnnbr = chn[0], chn[1:]
    if carlet.upper() not in lettres_possibles:
        return False
    if chnnbr not in chiffre_possible:
        return False
    return True

def lire_position(joueur):
    position = input(f"{joueur} dans quelle case voulez-vous jouer (Ex : A1) :")
    while not est_position_valide(position):
        print(f"{joueur} la case dans laquelle vous voulez jouer n'est pas valide.")
        position = str(input(f"{joueur}dans quelle case voulez-vous jouer (Ex : A1) :"))
    return position

def bateau_sur_case(pos):
    for key, val in dict_positions.items() :
        if pos in val :
            return key
    return None

def traduction_position(chn):
    lettre, chiffre = chn[0], chn[1:]
    lettre = lettre.upper()
    ligne = "ABCDEFGHIJ".index(lettre)
    colonne = ["1","2","3","4","5","6","7","8","9","10"].index(chiffre)
    return ligne,colonne

def valeur_case(pos):
    if bateau_sur_case(pos):
        return "X"
    return "O"

def modifier_case(pos, symbole): #attention pos est position traduite
    global grille
    ligne, colonne = traduction_position(pos)
    grille[ligne][colonne] = symbole
    return afficher_grille()


def est_coulé(bateau):
        lst = dict_positions[bateau]
        # print(lst)
        cmpt = 0
        for i in range(len(lst)):
            if lst[i] in lst_pos :
                cmpt += 1
        return cmpt == len(lst)

def modifier_bateau_coulé(bateau):
    global grille
    for elt in dict_positions[bateau] :
        ligne, colonne = traduction_position(elt)
        grille[ligne][colonne] = "*"
    return afficher_grille()


def tous_coulés():
    for key in dict_positions.keys():
        if not est_coulé(key):
            return False
    return True


# def tous_coulés():
#     nb_bateaux_coulés = 0
#     for key in dict_positions.keys():
#         if est_coulé(key):
#             nb_bateaux_coulés += 1
#         return nb_bateaux_coulés == 5
    
def jouer_un_tour(prénom):
    global lst_pos
    pos = lire_position(prénom)
    lst_pos.append(pos)
    print(modifier_case(pos, valeur_case(pos)))

def jouer_une_partie():
    prénom = str(input("Quel est votre prénom ? : "))
    afficher_grille()
    nb_torpilles = 30
    # while not tous_coulés() and nb_torpilles != 0:
    while True:
        jouer_un_tour(prénom)
        lst = []
        for key in dict_positions.keys():
            if est_coulé(key):
                modifier_bateau_coulé(key)
                lst.append(key)
                print(f"Vous avez coulé : {lst} !")
        print(f"Il vous reste {nb_torpilles} torpilles.")
        if tous_coulés():
            print("Bravo vous avez gagné, tous les bateaux ennemis ont été coulés !")
            break
        nb_torpilles -= 1
        if nb_torpilles == -1:
            print("Vous n'avez plus de torpilles, vous avez perdu")
            break

    


if __name__ == "__main__":
    grille = créer_grille_init()
    # afficher_grille()
    # # print(est_position_valide("A1"))  # True
    # print(est_position_valide("J10"))   # True
    # print(est_position_valide("L1"))    # False
    # print(est_position_valide("A11"))  # False        
    dict_positions = charger_bateaux("bateaux.csv")
    # prénom = "Camille"
    # print(lire_position(prénom))
    # print(dict_positions)
    # print(bateau_sur_case("A1"))
    # print(bateau_sur_case("A5"))
    # print(traduction_position("A1"))
    # print(traduction_position("j10"))
    # print(valeur_case("A1"))
    # print(valeur_case("D2"))
    # position = traduction_position("A1")
    # modifier_case(position, valeur_case("A1"))
    lst_pos = []
    # print(est_coulé("croiseur", lst_pos))
    # print(est_coulé("torpilleur", lst_pos))
    # print(jouer_un_tour())
    # print(est_coulé("porte-avions"))
    # print(modifier_bateau_coulé("porte-avions"))
    # print(dict_positions)
    jouer_une_partie()
    

