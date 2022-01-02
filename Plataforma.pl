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
user(Username,Password,Fecha, [Username,Password,Fecha]):-
        string(Username), string(Password), isDate(Fecha).

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
document(IdDoc,Creador,NombreDoc,TextoDoc,FechaDoc,VersionsDoc, [IdDoc,Creador,NombreDoc,TextoDoc,FechaDoc,VersionsDoc]):-
        number(IdDoc), string(Creador), string(NombreDoc), string(TextoDoc), isDate(FechaDoc), is_list(VersionsDoc), (IdDoc > 0; IdDoc = 0).

% Selectores de TDA Document
getIdDoc([IdDoc,_,_,_,_,_], IdDoc).
getCreadorDoc([_,Creador,_,_,_,_], Creador).
getNombreDoc([_,_,NombreDoc,_,_,_], NombreDoc).
getTextoDoc([_,_,_,TextoDoc,_,_], TextoDoc).
getFechaDoc([_,_,_,_,FechaDoc,_], FechaDoc).
getVersionsDoc([_,_,_,_,_,VersionsDoc], VersionsDoc).
getVersionID([IdVersion,_], IdVersion).
getVersionTexto([_,TextoVersion], TextoVersion).
getFirstElement([X|_],  X).
getLastElement([X], X):-!.
getLastElement([_|L], X) :- getLastElement(L, X).

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
%
getUserPassword([[Username,Password,_]|_],Username, Password):- !.
getUserPassword([[_,_,_]|Tail],Username,Password):-
        getUserPassword(Tail,Username,Password),!.
getLastIdDocument(Documentos, ID):-
        (Documentos = []),
        ID is -1, !.
getLastIdDocument(Documentos, ID):-
        getLastElement(Documentos, Lista),
        getFirstElement(Lista, ID).
% Pertenencia usuario
alreadyExists([[Username,_,_]|_], Username):- !.
alreadyExists([_|L], Username):-
            alreadyExists(L,Username).
% Otros, agregar un elemento a una lista cualquiera
addAux(_,Lista):-
        Lista=[], !.
addAux(H,[H2|T2]):-
        H\=H2, addAux(H, T2).

miembro(X, [Y|T]) :- X = Y; miembro(X, T), !.

addItemToList(Entrada, ListaOriginal, Salida):-
        number(Entrada), addAux(Entrada, ListaOriginal), append(ListaOriginal, [Entrada], Salida), !.
addItemToList([H|T], [H2|T2], Salida):-
        addAux(H, [H2|T2]), append([H2|T2], [H] , ListaAux), addItemToList(T, ListaAux, Salida), !.
addItemToList([_|T], ListaOriginal, Salida):-
        addItemToList(T,ListaOriginal,Salida), !.
addItemToList(_, ListaOriginal, Salida):- Salida = ListaOriginal.
% [0, "Nombre", ["Elem1", "Elem2"]], Comprobar si existe un permiso
existePermiso([[A,B,_]|_], A,B):- !.
existePermiso([_|L], A,B):-
        existePermiso(L,A,B).
% Buscar y agregar un permiso si existe, sino simplemente agregarlo
buscaryagregar(Entrada, _, ListaOriginal, Salida):-
        ListaOriginal=[], Salida = [Entrada],!.
buscaryagregar(Entrada, _, ListaOriginal, Salida):-
        getIdDocAccess(Entrada, ID), getUsernameAccess(Entrada, Nombre), not(existePermiso(ListaOriginal, ID, Nombre)),
        append(ListaOriginal, [Entrada], Salida), !.
buscaryagregar(Entrada, PrimeraParte, [[C,D,E]|F], Salida):-
        getIdDocAccess(Entrada, ID), getUsernameAccess(Entrada, Nombre), getListAccess(Entrada, Lista),
        ID=C,Nombre=D,addItemToList(E,Lista,Aux), append(PrimeraParte, [[ID,Nombre,Aux]], Aux2), append(Aux2, F, Aux3), sort(Aux3, Salida),!.
buscaryagregar(Entrada, PrimeraParte, [[C,D,E]|F], Salida):-
        getIdDocAccess(Entrada, ID), getUsernameAccess(Entrada, Nombre), getListAccess(Entrada, Lista),
        append([[C,D,E]], PrimeraParte, Aux), buscaryagregar([ID,Nombre,Lista], Aux, F, Salida).
% Auxiliar pal share
agregar(ID, Permisos, [H|T], ListaOriginal, Salida):-
        T=[], access(ID, H, Permisos, Acceso), buscaryagregar(Acceso, [], ListaOriginal, NuevoAccesos),
        Salida = NuevoAccesos, !.
agregar(ID, Permisos, [H|T], ListaOriginal, Salida):-
        access(ID, H, Permisos, Acceso), buscaryagregar(Acceso, [], ListaOriginal, NuevoAccesos),
        agregar(ID, Permisos, T, NuevoAccesos, Salida),!.

canWrite(ID, Nombre, [H|_], Nombre):-
        getIdDocAccess(H, IdActual), getUsernameAccess(H, NombreActual),
        ID = IdActual, Nombre = NombreActual,
        getListAccess(H, Accesos),
        miembro("w", Accesos),!.
canWrite(ID, Nombre, [_|T], Nombre):-
        canWrite(ID, Nombre, T, Nombre).
        canWrite(_, _, [_|T], Nombre):-
                T = [], Nombre = "".
% Auxiliar pal add el primero es si ya existe una version, el segundo pa la primera version, y el tercero pa ir avanzando
addTexto(ID, Date, Nombre, NuevoTexto, PrimeraParte, [Primero|T], _, Salida):-
        getIdDoc(Primero, IdDoc), getCreadorDoc(Primero, Creador),
        ID=IdDoc, Nombre=Creador, getTextoDoc(Primero, Texto),
        string_concat(Texto, NuevoTexto, TextoSalida),
        getVersionsDoc(Primero, VersionsDoc), getNombreDoc(Primero, NombreDoc),
        last(VersionsDoc, Last),
        getVersionID(Last, LastID),
        LastIdAux is LastID+1,
        append(VersionsDoc, [[LastIdAux, NuevoTexto]], VersionsNuevas), write(0),
        document(ID,Creador,NombreDoc,TextoSalida,Date,VersionsNuevas,Aux), append(PrimeraParte, [Aux], Aux2), append(Aux2, T, Salida), !.
addTexto(ID, Date, Nombre, NuevoTexto, PrimeraParte, [Primero|T], Accesos, Salida):-
        getIdDoc(Primero, IdDoc), getCreadorDoc(Primero, Creador), canWrite(ID, Nombre, Accesos, Usuario),
        ID=IdDoc, Usuario\="", getTextoDoc(Primero, Texto),
        string_concat(Texto, NuevoTexto, TextoSalida),
        getVersionsDoc(Primero, VersionsDoc), getNombreDoc(Primero, NombreDoc),
        last(VersionsDoc, Last),
        getVersionID(Last, LastID),
        LastIdAux is LastID+1,
        append(VersionsDoc, [[LastIdAux, NuevoTexto]], VersionsNuevas),
        document(ID,Creador,NombreDoc,TextoSalida,Date,VersionsNuevas,Aux), append(PrimeraParte, [Aux], Aux2), append(Aux2, T, Salida), !.
addTexto(ID, Date, Nombre, NuevoTexto, PrimeraParte, [H|T], Accesos, Salida):-
        append(PrimeraParte, [H], Aux), addTexto(ID, Date, Nombre, NuevoTexto, Aux, T, Accesos, Salida), !.


% Auxiliar pal restoreVersion
getVersionByID(ID, [H|_], Salida):-
        getVersionID(H, IdActual),
        ID = IdActual,Salida = H, !.
getVersionByID(_, [_|T], Salida):-
        T = [], Salida is -1, !.
getVersionByID(ID, [_|T], Salida):-
        getVersionByID(ID, T, Salida).
restoreVersion(ID, IdVersion, PrimeraParte, [Primero|T], Nombre, Salida):-
        getIdDoc(Primero, IdDoc), getCreadorDoc(Primero, Creador),
        ID=IdDoc, Nombre=Creador, getTextoDoc(Primero, TextoActual), getVersionsDoc(Primero, Versiones), getVersionByID(IdVersion, Versiones, VersionBuscada),
        getVersionTexto(VersionBuscada, NuevoTexto),
        getVersionsDoc(Primero, VersionsDoc), getNombreDoc(Primero, NombreDoc), getFechaDoc(Primero, Date),
        last(VersionsDoc, Last),
        getVersionID(Last, LastID),
        LastIdAux is LastID+1,
        append(VersionsDoc, [[LastIdAux, TextoActual]], VersionsNuevas),
        document(ID,Creador,NombreDoc,NuevoTexto,Date,VersionsNuevas,Aux), append(PrimeraParte, [Aux], Aux2), append(Aux2, T, Salida), !.
restoreVersion(ID, IdVersion, PrimeraParte, [Primero|T], Nombre, Salida):-
        getIdDoc(Primero, IdDoc), getCreadorDoc(Primero, Creador), canWrite(ID, Nombre, Accesos, Usuario),
        ID=IdDoc, Usuario\="", getTextoDoc(Primero, TextoActual), getVersionsDoc(Primero, Versiones), getVersionByID(IdVersion, Versiones, VersionBuscada),
        getVersionTexto(VersionBuscada, NuevoTexto),
        getVersionsDoc(Primero, VersionsDoc), getNombreDoc(Primero, NombreDoc), getFechaDoc(Primero, Date),
        last(VersionsDoc, Last),
        getVersionID(Last, LastID),
        LastIdAux is LastID+1,
        append(VersionsDoc, [[LastIdAux, TextoActual]], VersionsNuevas),
        document(ID,Creador,NombreDoc,NuevoTexto,Date,VersionsNuevas,Aux), append(PrimeraParte, [Aux], Aux2), append(Aux2, T, Salida), !.
restoreVersion(ID, IdVersion, PrimeraParte, [H|T], Nombre, Salida):-
        append(PrimeraParte, [H], Aux), restoreVersion(ID, IdVersion, Aux, T, Nombre, Salida).
%restoreVersion(1,1, [], [[0, "Pedro", "Doc1", "Texto", [1, 2, 2021], []], [1, "Juan", "Doc2", "Texto2Nuevo TextoNuevo Texto2NuevoTexto3", [4, 4, 2022], [[0, "Nuevo Texto"], [1, "Nuevo Texto2"], [2, "NuevoTexto3"]]]], "Juan", Salida), restoreVersion(1,0, [], Salida, "Juan", Salida2), restoreVersion(1,3, [], Salida2, "Juan", Salida3) ; true.
%Salida = [[0, "Pedro", "Doc1", "Texto", [1, 2, 2021], []], [1, "Juan", "Doc2", "Nuevo Texto2", [4, 4, 2022], [[0, "Nuevo Texto"], [1, "Nuevo Texto2"], [2, "NuevoTexto3"], [3, "Texto2Nuevo TextoNuevo Texto2NuevoTexto3"]]]],
%Salida2 = [[0, "Pedro", "Doc1", "Texto", [1, 2, 2021], []], [1, "Juan", "Doc2", "Nuevo Texto", [4, 4, 2022], [[0, "Nuevo Texto"], [1, "Nuevo Texto2"], [2, "NuevoTexto3"], [3, "Texto2Nuevo TextoNuevo Texto2NuevoTexto3"], [4, "Nuevo Texto2"]]]],
%Salida3 = [[0, "Pedro", "Doc1", "Texto", [1, 2, 2021], []], [1, "Juan", "Doc2", "Texto2Nuevo TextoNuevo Texto2NuevoTexto3", [4, 4, 2022], [[0, "Nuevo Texto"], [1, "Nuevo Texto2"], [2, "NuevoTexto3"], [3, "Texto2Nuevo TextoNuevo Texto2NuevoTexto3"], [4, "Nuevo Texto2"], [5, "Nuevo Texto"]]]] .

% Modificadores de TDA paradigmaDocs
addUserP(Sn1, User, Sn2):-
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        getOnlineUserP(Sn1, Online),
        append(Usuarios, [User], NuevaListaUsuarios),
        Sn2 = [NombreP, Fecha, NuevaListaUsuarios, Documentos, Accesos, Online], !.
setOnlineUser(Sn1, Username, Sn2):-
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        Sn2 = [NombreP, Fecha, Usuarios, Documentos, Accesos, Username], !.
% Funciones
paradigmaDocsRegister(Sn1, Fecha, Username, Password, Sn2):-
        getUsersP(Sn1, Usuarios),
        not(alreadyExists(Usuarios, Username)),
        user(Username, Password, Fecha, NuevoUsuario),
        addUserP(Sn1, NuevoUsuario, Sn2).

paradigmaDocsLogin(Sn1, Username, Password, Sn2):-
        getUsersP(Sn1, Usuarios),
        alreadyExists(Usuarios, Username),
        getUserPassword(Usuarios, Username, PasswordOut),
        PasswordOut = Password,
        setOnlineUser(Sn1, Username, Sn2).

paradigmaDocsCreate(Sn1, Fecha, Nombre, Contenido, Sn2):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getDocumentsP(Sn1, Documentos),
        getLastIdDocument(Documentos, ID),
        (ID = -1),
        ID2 is ID+1,
        document(ID2,OnlineUser,Nombre,Contenido,Fecha,[[0, Contenido]], NuevoDoc),
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getAccessesP(Sn1, Accesos),
        append(Documentos, [NuevoDoc], NuevosDocumentos),
        Sn2 = [NombreP, Fecha, Usuarios, NuevosDocumentos, Accesos, ""], !.

paradigmaDocsCreate(Sn1, Fecha, Nombre, Contenido, Sn2):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getDocumentsP(Sn1, Documentos),
        getLastIdDocument(Documentos, ID),
        ID2 is ID+1,
        document(ID2,OnlineUser,Nombre,Contenido,Fecha,[[0, Contenido]], NuevoDoc),
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getAccessesP(Sn1, Accesos),
        append(Documentos, [NuevoDoc], NuevosDocumentos),
        Sn2 = [NombreP, Fecha, Usuarios, NuevosDocumentos, Accesos, ""],!.

paradigmaDocsShare(Sn1, DocumentId, ListaPermisos, ListaUsernamesPermitidos, Sn2):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        agregar(DocumentId, ListaPermisos, ListaUsernamesPermitidos, Accesos, NuevoAccesos),
        Sn2 = [NombreP, Fecha, Usuarios, Documentos, NuevoAccesos, ""],!.

paradigmaDocsAdd(Sn1, DocumentId, Date, ContenidoTexto, Sn2):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        addTexto(DocumentId, Date, OnlineUser, ContenidoTexto, [], Documentos, Accesos, NuevoDocumentos),
        Sn2 = [NombreP, Fecha, Usuarios, NuevoDocumentos, Accesos, ""], !.

paradigmaDocsRestoreVersion(Sn1, DocumentId, IdVersion, Sn2):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        restoreVersion(DocumentId, IdVersion, [], Documentos, OnlineUser, NuevoDocumentos),
        Sn2 = [NombreP, Fecha, Usuarios, NuevoDocumentos, Accesos, ""], !.




/*paradigmaDocs("Plataforma", [1,2,2021], P1), paradigmaDocsRegister(P1, [1,2,2021], "Pedro", "Clave", P2),
paradigmaDocsLogin(P2, "Pedro", "Clave", P3), paradigmaDocsCreate(P3, [1,2,2021], "Archivo", "Contenido", P4),
paradigmaDocsLogin(P4, "Pedro", "Clave", P5),
paradigmaDocsShare(P5, 0, ["w", "r"], ["Juan"], P6),
paradigmaDocsLogin(P6, "Pedro", "Clave", P7),
paradigmaDocsAdd(P7, 0, [2,12,2022], "Nuevo Texto1", P8),
paradigmaDocsLogin(P8, "Pedro", "Clave", P9),
paradigmaDocsAdd(P9, 0, [2,12,2022], "Nuevo Texto2", P10),
paradigmaDocsLogin(P10, "Pedro", "Clave", P11),
paradigmaDocsAdd(P11, 0, [2,12,2022], "Nuevo Texto3", P12),
paradigmaDocsLogin(P12, "Pedro", "Clave", P13),
paradigmaDocsRestoreVersion(P13, 0, 3, P14).*/

/*paradigmaDocs("Plataforma", [1,2,2021], P1), paradigmaDocsRegister(P1, [1,2,2021], "Pedro", "Clave", P2),
paradigmaDocsLogin(P2, "Pedro", "Clave", P3), paradigmaDocsCreate(P3, [1,2,2021], "Archivo", "Contenido", P4),
paradigmaDocsLogin(P4, "Pedro", "Clave", P5),
paradigmaDocsShare(P5, 0, ["w", "r"], ["Juan"], P6),
paradigmaDocsRegister(P6, [1,2,2021], "Juan", "Clave", P7),
paradigmaDocsLogin(P7, "Juan", "Clave", P8),
paradigmaDocsAdd(P8, 0, [2,12,2022], "Texto de Juan", P9),
paradigmaDocsRegister(P9, [1,2,2021], "Diego", "Clave", P10),
paradigmaDocsLogin(P10, "Diego", "Clave", P11),
paradigmaDocsAdd(P11, 0, [2,12,2022], "Texto de Diego", P12).
*/
