var __langs = {
  "fr": {
    "Main room":
      "Salon principal",
    "Invalid password":
      "Mot de passe incorrect",
    "Login informations":
      "Informations de connexion",
    "User name":
      "Nom d'utilisateur",
    "Password":
      "Mot de passe",
    "Room":
      "Salon",
    "Room Password":
      "Mot de passe du salon",
    "Nick":
      "Pseudo",
    "Login":
      "S'authentifier",
    "Authorisation...":
      "Autorisation...",
    "Joining Room...":
      "Connexion au salon...",
    "You has been disconnected":
      "Vous avez été déconnecté",
    "This room requires password":
      "Ce salon nécessite un mot de passe",
    "You are banned from this room":
      "Vous avez été exclu de ce salon",
    "This room doesn't exist":
      "Ce salon n'exite pas",
    "This room doesn't exist, and can be created only by administrator":
      "Ce salon n'existe pas et peut être créé uniquement par un administrateur",
    "This room can be accessed only by registered persons":
      "Ce salon n'est accessible qu'aux personnes enregistrées",
    "You are not member of this room":
      "Vous n'êtes pas membre de ce salon",
    "You nick name is already used, try another nick":
      "Votre pseudo est déjà utilisé, essayez en un autre",
    "Conference server can't be contacted or room reached maximum number of users":
      "Le serveur de discussion n'est pas trouvé ou le nombre maximal d'utilisateurs du salon a été atteint",
    "Can't join conference room":
      "Ne peut pas rejoindre le salon",
    "Invalid login information":
      "Informations de connexion invalides",
    "{0} has joined this room":
      "{0} a rejoint le salon",
    "{0} has left this room":
      "{0} a quitté le salon",
    "{0} has left this room: {1}":
      "{0} a quitté le salon: {1}",
    "{0} changed his nick to {1}":
      "{0} a changé son pseudo {1}",

    "You has been kicked from the room by {0}: {1}":
      "Vous avez été éjecté du salon par {0}: {1}",
    "You has been kicked from the room by {0}":
      "Vous avez été éjecté du salon par {0}",
    "You has been kicked from the room: {0}":
      "Vous avez été éjecté du salon: {0}",
    "You has been kicked from the room":
      "Vous avez été éjecté du salon",

    "You has been banned from the room by {0}: {1}":
      "Vous avez été banni du salon par {0}: {1}",
    "You has been banned from the room by {0}":
      "Vous avez été banni du salon par {0}",
    "You has been banned from the room: {0}":
      "Vous avez été banni du salon: {0}",
    "You has been banned from the room":
      "Vous avez été banni du salon",

    "{0} has been kicked from the room by {1}: {2}":
      "{0} a été éjecté du salon par {1}: {2}",
    "{0} has been kicked from the room by {1}":
      "{0} a été éjecté du salon par {1}",
    "{0} has been kicked from the room: {1}":
      "{0} a été éjecté du salon: {1}",
    "{0} has been kicked from the room":
      "{0} a été éjecté du salon",

    "{0} has been banned from the room by {1}: {2}":
      "{0} a été banni du salon par {1}: {2}",
    "{0} has been banned from the room by {1}":
      "{0} a été banni du salon par {1}",
    "{0} has been banned from the room: {1}":
      "{0} a été banni du salon: {1}",
    "{0} has been banned from the room":
      "{0} a été banni du salon",

    "Moderators":
      "Modérateurs",
    "Participants":
      "Participants",
    "Visitors":
      "Visiteurs",
    "No role assigned":
      "Aucun rôle défini",

    "Actions":
      "Actions",
    "Ban User":
      "Bannir un utilisateur",
    "Kick User":
      "Ejecter un utilisateur",
    "Jabber ID":
      "Identifiant Jabber",
    "Reason":
      "Raison",
    "Cancel":
      "Annuler",

    "Yesterday":
      "Hier",
    "Sunday":
      "Dimanche",
    "Monday":
      "Lundi",
    "Tuesday":
      "Mardi",
    "Wednesday":
      "Mercredi",
    "Thursday":
      "Jeudi",
    "Friday":
      "Vendredi",
    "Saturday":
      "Samedi",

    "Remove Bans":
      "",
    "Add Ban":
      "",
    "Add ban":
      "",
    "Save":
      "",
    "Ban List Editor":
      "",
    "Edit Bans List":
      "",

    "Pause":
      "",
    "Blocked Words":
      "",
    "Mute by nickname":
      "",
    "Mute":
      "",
    "User with nick '{0}' doesn't exist":
      "",
    "Blocked word":
      "",
    "Block":
      "",
    "Word '{0}' is already blocked":
      "",
    "Unblock":
      "",
    "Word '{0}' is not blocked":
      "",
    "User with nick '{0}' is already muted":
      "",
    "User with nick '{0}' is already unmuted":
      ""
  }
}

function _(string) {
  var map = __langs[parameters.lang];
  string = map && map[string] || string;
  return arguments.length > 1 ? String.format.apply(String, arguments) : string;
}
