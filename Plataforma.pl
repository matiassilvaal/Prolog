% --TDAs--
/*
TDA date.
D: Un numero que representa el dia.
M: Un numero que representa el mes.
A: Un numero que representa el año.
Entrega en el ultimo parametro una lista con estas 3 variables, siempre que cumplan las condiciones
definidas para cada una.
*/
enero(D,M)     :- M = 1 , (D < 31 ; D = 31).
febrero(D,M)   :- M = 2 , (D < 29 ; D = 29).
marzo(D,M)     :- M = 3 , (D < 31 ; D = 31).
abril(D,M)     :- M = 4 , (D < 30 ; D = 30).
mayo(D,M)      :- M = 5 , (D < 31 ; D = 31).
junio(D,M)     :- M = 6 , (D < 30 ; D = 30).
julio(D,M)     :- M = 7 , (D < 31 ; D = 31).
agosto(D,M)    :- M = 8 , (D < 31 ; D = 31).
septiembre(D,M):- M = 9 , (D < 30 ; D = 30).
octubre(D,M)   :- M = 10, (D < 31 ; D = 31).
noviembre(D,M) :- M = 11, (D < 30 ; D = 30).
diciembre(D,M) :- M = 12, (D < 31 ; D = 31).
date(D,M,Y, [D,M,Y]):-
            number(D),number(M),number(Y), !, (D > 0, M > 0, Y > 0),
            (M < 12; M = 12),
            (enero(D,M);febrero(D,M); marzo(D,M);
            abril(D,M); mayo(D,M);junio(D,M);
            julio(D,M); agosto(D,M); septiembre(D,M);
            octubre(D,M);noviembre(D,M);diciembre(D,M)),!.

/*
Predicado de pertencia al TDA date, funciona si el constructor es capaz de crear una fecha
Dominio: date List
*/
isDate([D,M,Y]) :-
        date(D,M,Y,_).

/*
TDA user.
Username: Un string que representa el nombre de usuario
Password: Un string que representa la contraseña del usuario
Fecha: un date que representa la fecha de creación
Entrega en el ultimo parametro una lista con estas 3 variables, siempre que cumplan las condiciones
definidas para cada una.
*/
user(Username,Password,Fecha, [[Username,Password,Fecha]]):-
        string(Username), string(Password), isDate(Fecha).

/*
Predicado de pertencia al TDA user, funciona si el constructor es capaz de crear un user
Dominio: user List
*/
isUser([Username,Password,Fecha]) :-
        user(Username,Password,Fecha,_).

% Selectores de TDA user
getUsername([Nombre,_,_], Nombre).
getPassword([_,Password,_], Password).
getUserCreation([_,_,Fecha], Fecha).

/*
TDA document.
IdDoc: Un numero que representa el ID del documento
NombreDoc: Un string que representa el nombre del documento
TextoDoc: Un string que representa el texto del documento
FechaDoc: Un date que representa la fecha del documento
VersionsDoc: una lista que incluye una ID de version y el texto de la misma
Entrega en el ultimo parametro una lista con estas 5 variables, siempre que cumplan las condiciones
definidas para cada una.
*/
document(IdDoc,NombreDoc,TextoDoc,FechaDoc,VersionsDoc, [IdDoc,NombreDoc,TextoDoc,FechaDoc,VersionsDoc]):-
        number(IdDoc), string(NombreDoc), string(TextoDoc), isDate(FechaDoc), is_list(VersionsDoc), (IdDoc > 0; IdDoc = 0).

/*
Predicado de pertencia al TDA document, funciona si el constructor es capaz de crear un document
Dominio: document List
*/
isDocument([IdDoc,NombreDoc,TextoDoc,FechaDoc,VersionsDoc]) :-
        document(IdDoc,NombreDoc,TextoDoc,FechaDoc,VersionsDoc,_).

% Selectores de TDA user
getIdDoc([IdDoc,_,_,_,_], IdDoc).
getNombreDoc([_,NombreDoc,_,_,_], NombreDoc).
getTextoDoc([_,_,TextoDoc,_,_], TextoDoc).
getFechaDoc([_,_,_,FechaDoc,_], FechaDoc).
getVersionsDoc([_,_,_,_,VersionsDoc], VersionsDoc).
getVersionID([IdVersion|_], IdVersion).
getFirstVersion([X|_],  X).
getLastVersion([X], X):-!.
getLastVersion([_|L], X) :- getLastVersion(L, X).

/*
TDA access.
IdDocAccess: Un numero que representa el ID del documento a la que se tiene acceso
UsernameAccess: Un string que representa al usuario que tiene acceso
ListAccess: Un list que representa los accesos que tiene un usuario ej: ["W", "R"]
Entrega en el ultimo parametro una lista con estas 3 variables, siempre que cumplan las condiciones
definidas para cada una.
*/
access(IdDocAccess, UsernameAccess, ListAccess, [IdDocAccess, UsernameAccess, ListAccess]):-
        number(IdDocAccess), string(UsernameAccess), is_list(ListAccess).

/*
Predicado de pertencia al TDA access, funciona si el constructor es capaz de crear un access
Dominio: access List
*/
isAccess([IdDocAccess, UsernameAccess, ListAccess]):-
        access(IdDocAccess, UsernameAccess, ListAccess,_).

% Selectores de TDA access
getIdDocAccess([IdDocAccess,_,_], IdDocAccess).
getUsernameAccess([_,UsernameAccess,_], UsernameAccess).
getListAccess([_,_,ListAccess], ListAccess).

/*
TDA paradigmaDocs.
Name: Un string que representa el nombre de la plataforma
Date: un date que representa la fecha de creacion
SOut: un paradigmadocs que representa la plataforma de salida
Entrega en el ultimo parametro una lista con estas 3 variables, siempre que cumplan las condiciones
definidas para cada una.
*/
paradigmaDocs(Name, Date, SOut) :-
        string(Name), isDate(Date), SOut = [Name, Date, [], [], [], ""].

% Selectores de TDA paradigmaDocs
getNameP([Name,_,_,_,_,_], Name).
getDateP([_,Date,_,_,_,_], Date).
getUsersP([_,_,Users,_,_,_], Users).
getDocumentsP([_,_,_,Documents,_,_], Documents).
getAccessesP([_,_,_,_,Accesses,_], Accesses).
getOnlineUserP([_,_,_,_,_,User], User).

% Pertenencia usuario
alreadyExists([[Username,_,_]|_], Username):- !.
alreadyExists([[_,_,_]|L], Username):-
            alreadyExists(L,Username).

% Modificadores de TDA paradigmaDocs
addUserP(Sn1, User, Sn2):-
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        getOnlineUserP(Sn1, Online),
        append(Usuarios, User, NuevaListaUsuarios),
        Sn2 = [NombreP, Fecha, NuevaListaUsuarios, Documentos, Accesos, Online], !.

% Funciones
paradigmaDocsRegister(Sn1, Fecha, Username, Password, Sn2):-
        getUsersP(Sn1, Usuarios),
        not(alreadyExists(Usuarios, Username)),
        user(Username, Password, Fecha, NuevoUsuario),
        addUserP(Sn1, NuevoUsuario, Sn2).
