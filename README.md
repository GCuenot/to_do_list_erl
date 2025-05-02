# to_do_list_erl
commandes à faire :

serveur :
erl -sname nom_serveur -setcookie mycookie
db_manager:start(init).

client :
erl -sname nom_client -setcookie mycookie
db_manager:start(join).

db_manager:connect_to(nom_serveur@nom_serveur).
db_manager:get_all_events().
db_manager:register_user("nom","mdp").
db_manager:add_event(date, "jour", "heure", "titre", "user").

problèmes : 
Pas de connexion entre deux pc, problèmes avec la bdd.
