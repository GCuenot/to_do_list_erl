%% event.hrl

-record(event, {
    id,             % Identifiant unique de l'événement
    jour,           % Jour (lundi, mardi, etc.)
    heure,          % Heure (format "HH:MM")
    titre,          % Titre de l'événement
    utilisateur     % Créateur de l'événement
}).
