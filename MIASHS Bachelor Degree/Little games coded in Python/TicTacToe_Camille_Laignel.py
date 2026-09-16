import random


def créer_grille_vide():
    return [[" "," "," "],
            [" "," "," "],
            [" "," "," "]]

def afficher_grille():
    print(f"3 {grille[0][0]} | {grille[0][1]} | {grille[0][2]}")
    print(" -----------")
    print(f"2 {grille[1][0]} | {grille[1][1]} | {grille[1][2]}")
    print(" -----------")
    print(f"1 {grille[2][0]} | {grille[2][1]} | {grille[2][2]}")
    print("   A  B  C")
    
def est_grille_pleine():
    for i in range(3):
        for j in range(3):
            if grille[i][j] == " " :
                return False
    return True

def alignement_ligne():
    for ligne in grille:
            if ligne[0] == ligne[1] == ligne[2] and ligne[0]!=" " :
                return True
    return False
            
def alignement_colonne():
    for j in range(3):
        if grille[0][j] == grille[1][j] == grille[2][j] and grille[0][j] != " " :
            return True
    return False

def alignement_diagonale():
    if grille[0][0] == grille[1][1] == grille[2][2] and grille[0][0] != " " :
        return True
    if grille[0][2] == grille[1][1] == grille[2][0] and grille[2][0] != " " :
        return True
    return False

def alignement():
    return alignement_colonne() or alignement_diagonale() or alignement_ligne()

def est_position_valide(chn):
    lettre_possible = "ABC"
    chiffre_possible = "123"
    lettre, chiffre = chn
    # print(lettre, chiffre)
    if (lettre.upper() in lettre_possible) and (chiffre in chiffre_possible) :
        return True
    return False

def traduction_position(chn):
    lettre,chiffre = chn
    lettre = lettre.upper()
    ligne = "321".index(chiffre)
    colonne = "ABC".index(lettre)
    return (ligne,colonne)


def est_position_libre(chn):
    ligne, colonne = traduction_position(chn)
    return grille[ligne][colonne] == " "

def est_position_correcte(chn):
    return est_position_libre(chn) and est_position_valide(chn)

def lire_prénom():
    prénom = input("Quel est votre prénom ? :")
    return prénom

def qui_commence():
    return random.randint(0,1)
    

def joueur_suivant(joueur):
    if joueur == 0:
        return 1
    else :
        return 0
    
def lire_position(prénom):
    position = str(input(f"{prénom} dans quelle case voulez-vous jouer (Ex : A1) :"))
    while not est_position_correcte(position):
        print(f"{prénom} la case dans laquelle vous voulez jouer n'est pas disponible.")
        position = str(input(f"{prénom}dans quelle case voulez-vous jouer (Ex : A1) :"))
    return position

def placer_pion(pos, symbole):
    ligne, colonne = pos
    grille[ligne][colonne] = symbole
    print(afficher_grille())

def jouer_un_tour(joueur,lst_prénoms):
    afficher_grille()
    pos = traduction_position(lire_position(lst_prénoms[joueur]))
    if joueur == 0 :
        symbole = "X"
        return placer_pion(pos, symbole)
    else :
        symbole = "O"
        return placer_pion(pos, symbole)
    
def jouer_une_partie(lst_prénoms):
    joueur = qui_commence()
    jouer_un_tour(joueur,lst_prénoms)
    while not est_grille_pleine() and not alignement():
        joueur =joueur_suivant(joueur)
        jouer_un_tour(joueur,lst_prénoms)
    if alignement():
        afficher_grille()
        print(f"{lst_prénoms[joueur]} vous avez gagné !")
        return joueur
    elif est_grille_pleine() :
        afficher_grille()
        print("Match nul.")
        return -1
    
def jouer():
    afficher_grille()
    joueur_1 = lire_prénom()
    joueur_2 = lire_prénom()
    lst_prénoms = [joueur_1,joueur_2]
    pts_j1, pts_j2 = 0,0
    matchs_nuls = 0
    réponse = "oui"
    while réponse == "oui":
        for i in range(3):
            for j in range(3):
                grille[i][j] = " "
        résultat = jouer_une_partie(lst_prénoms)
        if résultat == 0 :
            pts_j1 += 1
        elif résultat == 1:
            pts_j2 += 1
        else :
            matchs_nuls += 1
        print(f"{joueur_1} a {pts_j1} points. \n {joueur_2} a {pts_j2} points. \n Vous avez fait {matchs_nuls} matchs nuls.")
        print()
        réponse = str(input("Voulez-vous rejouer une partie ? (oui/non) :"))
    print(f"La partie est finie, voici les scores : \n {joueur_1} : {pts_j1} \n {joueur_2} : {pts_j2} \n Matchs nuls : {matchs_nuls}")

    


    




if __name__ == '__main__':
    grille = créer_grille_vide()
    # afficher_grille()
    #print(est_grille_pleine())
    #après cette fonction faire une boucle de test
    # print(alignement())
    # print(est_position_valide())
    # chn = input("Dans quelle case voulez-vous jouer ? :")
    # while not est_position_valide(chn):
    #     print("Attention vous devez donner une lettre (ABC) puis un chiffre (123) :")
    #     chn = input("Dans quelle case voulez-vous jouer ? :")
    # coord = traduction_position(chn)
    # print(est_position_correcte())
    # print(lst_prénoms)
    # joueur = qui_commence()
    # position = lire_position(joueur)
    # print(lire_position(joueur))
    jouer()

