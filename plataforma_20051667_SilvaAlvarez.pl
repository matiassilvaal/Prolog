% --TDAs--
/*
                TDA date.
% Dominios:
        D: entero
        M: entero
        Y: entero
% Predicados y Metas:
        Predicados con metas principales:
                date(D,M,Y, [D,M,Y]).
        Predicados con metas secundarias:
                enero(D,M).
                febrero(D,M).
                marzo(D,M).
                abril(D,M).
                mayo(D,M).
                junio(D,M).
                julio(D,M).
                agosto(D,M).
                septiembre(D,M).
                octubre(D,M).
                noviembre(D,M).
                diciembre(D,M).
                isDate([D,M,Y]).
                getDia([D,_,_], D).
                getMes([_,M,_], M).
                getAnio([_,_,A], A).
*/
%               Clausulas
% Comprueba que una fecha ingresada sea correcta para un mes
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
% Crea un objeto tipo date que es una lista con el dia, mes y año
date(D,M,Y, [D,M,Y]):-
            number(D),number(M),number(Y), !, (D > 0, M > 0, Y > 0),
            (M < 12; M = 12),
            (enero(D,M);febrero(D,M); marzo(D,M);
            abril(D,M); mayo(D,M);junio(D,M);
            julio(D,M); agosto(D,M); septiembre(D,M);
            octubre(D,M);noviembre(D,M);diciembre(D,M)),!.
% Pertenencia del tipo date
isDate([D,M,Y]) :-
        date(D,M,Y,_).
% Selectores del tipo date
getDia([D,_,_], D).
getMes([_,M,_], M).
getAnio([_,_,A], A).

/*
                TDA user.
% Dominios:
        Username: string
        Password: string
        Fecha: date
% Predicados y Metas:
        Predicados con metas principales:
                user(Username,Password,Fecha, [Username,Password,Fecha]).
        Predicados con metas secundarias:
                getUsername([Nombre,_,_], Nombre).
                getPassword([_,Password,_], Password).
                getUserCreation([_,_,Fecha], Fecha).
*/
%               Clausulas
% Crea un objeto tipo user, que es una lista con el usuario, su contraseña y la fecha de creacion
user(Username,Password,Fecha, [Username,Password,Fecha]):-
        string(Username), string(Password), isDate(Fecha).
% Selectores de TDA user
getUsername([Nombre,_,_], Nombre).
getPassword([_,Password,_], Password).
getUserCreation([_,_,Fecha], Fecha).

/*
                TDA document.
% Dominios:
        IdDoc: entero
        Creador: string
        NombreDoc: string
        TextoDoc: string
        FechaDoc: date
        VersionsDoc: [entero, string]
        H: primer elemento de una lista
        T: cola de una lista
% Predicados y Metas:
        Predicados con metas principales:
                document(IdDoc,Creador,NombreDoc,TextoDoc,FechaDoc,VersionsDoc, [IdDoc,Creador,NombreDoc,TextoDoc,FechaDoc,VersionsDoc]).
        Predicados con metas secundarias:
                getIdDoc([IdDoc,_,_,_,_,_], IdDoc).
                getCreadorDoc([_,Creador,_,_,_,_], Creador).
                getNombreDoc([_,_,NombreDoc,_,_,_], NombreDoc).
                getTextoDoc([_,_,_,TextoDoc,_,_], TextoDoc).
                getFechaDoc([_,_,_,_,FechaDoc,_], FechaDoc).
                getVersionsDoc([_,_,_,_,_,VersionsDoc], VersionsDoc).
                getVersionID([IdVersion,_], IdVersion).
                getVersionTexto([_,TextoVersion], TextoVersion).
                getVersionByID(ID, [H|T], Salida).
                getVersionByID(_, [_|T], Salida).
                getVersionByID(ID, [_|T], Salida).
*/
%               Clausulas
% Crea un objeto tipo documento, que es una lista con la ID, el creador, el nombre, el contenido, la fecha de creacion y las versiones del documento.
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
% Obtiene recursivamente una version especifica de un documento especifico en base a su ID
getVersionByID(ID, [H|_], Salida):-
        getVersionID(H, IdActual),
        ID = IdActual,Salida = H, !.
getVersionByID(_, [_|T], Salida):-
        T = [], Salida is -1, !.
getVersionByID(ID, [_|T], Salida):-
        getVersionByID(ID, T, Salida).

/*
                TDA access.
% Dominios:
        IdDocAccess: entero
        UsernameAccess: entero
        ListAccess: [string, string, string, ..., string]
% Predicados y Metas:
        Predicados con metas principales:
                access(IdDocAccess, UsernameAccess, ListAccess, [IdDocAccess, UsernameAccess, ListAccess]).
        Predicados con metas secundarias:
                getIdDocAccess([IdDocAccess,_,_], IdDocAccess).
                getUsernameAccess([_,UsernameAccess,_], UsernameAccess).
                getListAccess([_,_,ListAccess], ListAccess).
*/
%               Clausulas
% Crea un objeto tipo access, que tiene una ID, un usuario y una lista de permisos
access(IdDocAccess, UsernameAccess, ListAccess, [IdDocAccess, UsernameAccess, ListAccess]):-
        number(IdDocAccess), string(UsernameAccess), is_list(ListAccess).
% Selectores de TDA access
getIdDocAccess([IdDocAccess,_,_], IdDocAccess).
getUsernameAccess([_,UsernameAccess,_], UsernameAccess).
getListAccess([_,_,ListAccess], ListAccess).
/*
                TDA paradigmaDocs.
% Dominios:
        Name: string
        Date: date
        SOut: paradigmadocs
        Users: user List
        Documents: document List
        Accesses: access List
        H: primer elemento de una lista
        T: cola de una lista
        Username: string
        H: primer elemento de una Lista
        Lista: list
        H2: primer elemento de una lista
        T2: cola de una lista
        X: cualquier tipo de elemento (string, entero, lista, etc)
        ListaOriginal: List
        Salida: list
        Sn1: paradigmadocs
        Sn2: paradigmadocs
        Fecha: date
        Password: string
        Contenido: string
        Entrada: access
        ListaOriginal: string list (Para buscaryagregar y agregar)
        Salida: accesses list (Para buscaryagregar y agregar)
        ID: entero
        Documentos: documents list
        Usuario: string
        Accesos: accesses list
        Permisos: string list
        DocumentId: entero
        ListaPermisos: string list
        ListaUsernamesPermitidos: string list
        Date: fecha
        Nombre: string
        NuevoTexto: string
        PrimeraParte: documents list
        Primero: primer elemento de una lista
        Salida: documents list (Para addTexto)
        ContenidoTexto: string
        IdVersion: entero
        Usuarios: users list
        Texto: string
        UsuariosSalida: string
        Versiones: Versiones list
        VersionesSalida: string
        DocumentosSalida: string
        Permisos: access (Para obtenerPermisos)
        PermisosSalida: string
        AccesosSalida: string
        NombreP: string
        StrOut: string
        Salida: string (Para obtenerUsuarioInfo, obtenerDocumentosUsuario, obtenerAccesosUsuario)
% Predicados y Metas:
        Predicados con metas principales:
                paradigmaDocs(Name, Date, SOut).
        Predicados con metas secundarias:
                getNameP([Name,_,_,_,_,_], Name).
                getDateP([_,Date,_,_,_,_], Date).
                getUsersP([_,_,Users,_,_,_], Users).
                getDocumentsP([_,_,_,Documents,_,_], Documents).
                getAccessesP([_,_,_,_,Accesses,_], Accesses).
                getOnlineUserP([_,_,_,_,_,User], User).
                getLastIdDocument(Documentos, ID).
                getUserPassword([[Username,Password,_]|_],Username, Password).
                alreadyExists([[Username,_,_]|_], Username).
                existePermiso([[ID,Username,_]|_], ID,Username).
                esDuenio([[ID,Username,_,_,_,_]|_], ID, Username).
                canShare(ID, Username, [H|_]).
                canWrite(ID, Username, [H|_], Username).
                getFirstElement([H|_],  H).
                getOthers([_|T], T).
                getLastElement([H], H).
                addAux(_,Lista).
                addItemToList(Entrada, ListaOriginal, Salida).
                addUserP(Sn1, User, Sn2).
                register(Sn1, Fecha, Username, Password, Sn2).
                setOnlineUser(Sn1, Username, Sn2).
                login(Sn1, Username, Password, Sn2).
                create(Sn1, Fecha, Nombre, Contenido, Sn2).
                buscaryagregar(Entrada, _, ListaOriginal, Salida).
                agregar(ID, Documentos, Usuario, Accesos, Permisos, [H|T], ListaOriginal, Salida).
                share(Sn1, DocumentId, ListaPermisos, ListaUsernamesPermitidos, Sn2).
                addTexto(ID, Date, Nombre, NuevoTexto, PrimeraParte, [Primero|T], _, Salida).
                add(Sn1, DocumentId, Date, ContenidoTexto, Sn2).
                restoreVersion(ID, IdVersion, PrimeraParte, [Primero|T], _, Nombre, Salida).
                restore(Sn1, DocumentId, IdVersion, Sn2).
                obtenerUsuarios(Usuarios, Texto, UsuariosSalida).
                obtenerVersiones(Versiones, Texto, VersionesSalida).
                obtenerDocumentos(Documentos, Texto, DocumentosSalida).
                obtenerPermisos(Permisos, Texto, PermisosSalida).
                obtenerAccesos(Accesos, Texto, AccesosSalida).
                escribirTodo(NombreP, Fecha, Usuarios, Documentos, Accesos, StrOut).
                obtenerUsuarioInfo(Usuarios, Nombre, Salida).
                obtenerDocumentosUsuario(Documentos, Nombre, Texto, Salida).
                obtenerAccesosUsuario(Accesos, Nombre, Texto, Salida).
                escribirUsuario(Usuario, NombreP, Fecha, Usuarios, Documentos, Accesos, StrOut).
                toString(Sn1, StrOut).
*/
%               Clausulas
% Crea un objeto paradigmadocs, esto es una lista con el nombre de la plataforma, la fecha de creacion, una lista para los usuarios, una lista para los documentos, una lista para los accesos y un string del usuario conectado.
paradigmaDocs(Name, Date, SOut) :-
        string(Name), isDate(Date), SOut = [Name, Date, [], [], [], ""].
% Selectores de TDA paradigmaDocs
getNameP([Name,_,_,_,_,_], Name).
getDateP([_,Date,_,_,_,_], Date).
getUsersP([_,_,Users,_,_,_], Users).
getDocumentsP([_,_,_,Documents,_,_], Documents).
getAccessesP([_,_,_,_,Accesses,_], Accesses).
getOnlineUserP([_,_,_,_,_,User], User).
% Selector que obtiene la ultima ID de la lista de documentos de paradigmadocs
% Si no existen documentos, la ID es -1, si existe almenos uno se obtiene la ultima ID
getLastIdDocument(Documentos, ID):-
        (Documentos = []),
        ID is -1, !.
getLastIdDocument(Documentos, ID):-
        getLastElement(Documentos, Lista),
        getFirstElement(Lista, ID).
% Selector que obtiene la password de un usuario de la lista de usuarios de paradigmadocs
getUserPassword([[Username,Password,_]|_],Username, Password):- !.
getUserPassword([[_,_,_]|T],Username,Password):-
        getUserPassword(T,Username,Password),!.
% Pertenencia de un user en la lista de usuarios de paradigmadocs
alreadyExists([[Username,_,_]|_], Username):- !.
alreadyExists([_|T], Username):-
            alreadyExists(T,Username).
% Obtiene recursivamente si un permiso existe en una lista de accesos de paradigmadocs
existePermiso([[ID,Username,_]|_], ID,Username):- !.
existePermiso([_|T], ID,Username):-
        existePermiso(T,ID,Username).
% Obtiene recursivamente si un usuario es dueño de un documento, dentro de una lista de documentos de paradigmadocs
esDuenio([[ID,Username,_,_,_,_]|_], ID, Username):- !.
esDuenio([_|T], ID, Username):-
            esDuenio(T, ID, Username).
% Obtiene recursivamente si un par ID, Username existe en una lista de accesos de paradigmadocs y si este tiene el permiso de compartir
canShare(ID, Username, [H|_]):-
        getIdDocAccess(H, IdActual), getUsernameAccess(H, NombreActual),
        ID = IdActual, Username = NombreActual,
        getListAccess(H, Accesos),
        miembro("s", Accesos),!.
canShare(ID, Username, [_|T]):-
        canShare(ID, Username, T), !.
% Obtiene recursivamente si un par ID, Username existe en una lista de accesos de paradigmadocs y si este tiene el permiso de escribir
canWrite(ID, Username, [H|_], Username):-
        getIdDocAccess(H, IdActual), getUsernameAccess(H, NombreActual),
        ID = IdActual, Username = NombreActual,
        getListAccess(H, Accesos),
        miembro("w", Accesos),!.
canWrite(ID, Username, [_|T], Username):-
        canWrite(ID, Username, T, Username).
canWrite(_, _, [_|T], Username):-
        T = [], Username = "".
% Obtiene el primer elemento de una lista cualquiera
getFirstElement([H|_],  H).
% Obtiene todos los elementos de una lista salvo el primero
getOthers([_|T], T).
% Obtiene recursivamente el ultimo elemento de una lista
getLastElement([H], H):-!.
getLastElement([_|T], E) :- getLastElement(T, E).
% Agrega elementos recursivamente a una lista si la entrada es distinto al elemento actual
addAux(_,Lista):-
        Lista=[], !.
addAux(H,[H2|T2]):-
        H\=H2, addAux(H, T2).
% Comprueba si X pertenece a una lista cualquiera recursivamente
miembro(X, [H|T]) :- X = H; miembro(X, T), !.
% Agrega una entrada a una lista cualquiera si los elementos de entrada no existen en la lista original, entrega una lista modificada con los elementos nuevos y los antiguos
addItemToList(Entrada, ListaOriginal, Salida):-
        number(Entrada), addAux(Entrada, ListaOriginal), append(ListaOriginal, [Entrada], Salida), !.
addItemToList([H|T], [H2|T2], Salida):-
        addAux(H, [H2|T2]), append([H2|T2], [H] , ListaAux), addItemToList(T, ListaAux, Salida), !.
addItemToList([_|T], ListaOriginal, Salida):-
        addItemToList(T,ListaOriginal,Salida), !.
addItemToList(_, ListaOriginal, Salida):- Salida = ListaOriginal.
% Retorna un paradigmadocs con un nuevo usuario
addUserP(Sn1, Username, Sn2):-
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        getOnlineUserP(Sn1, Online),
        append(Usuarios, [Username], NuevaListaUsuarios),
        Sn2 = [NombreP, Fecha, NuevaListaUsuarios, Documentos, Accesos, Online], !.
% Comprueba que el usuario que se quiera agregar al paradigmadocs no existe, el resultado es un paradigmadocs actualizado si no existe o false si existe el usuario
register(Sn1, Fecha, Username, Password, Sn2):-
        getUsersP(Sn1, Usuarios),
        not(alreadyExists(Usuarios, Username)),
        user(Username, Password, Fecha, NuevoUsuario),
        addUserP(Sn1, NuevoUsuario, Sn2).
% El resultado es un paradigmadocs con un usuario activo
setOnlineUser(Sn1, Username, Sn2):-
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        Sn2 = [NombreP, Fecha, Usuarios, Documentos, Accesos, Username], !.
% Si un usuario existe en el paradigmadocs, lo logea y lo deja activo, entrega una version actualizada con el usuario activo, si el logeo falla, entrega falso
login(Sn1, Username, Password, Sn2):-
        getUsersP(Sn1, Usuarios),
        alreadyExists(Usuarios, Username),
        getUserPassword(Usuarios, Username, PasswordOut),
        PasswordOut = Password,
        setOnlineUser(Sn1, Username, Sn2).
% Si no existe un usuario logeado devuelve falso, si existe uno, el resultado es paradigmadocs actualizado con un nuevo documento y sin usuario activo (se cierra la sesion)
% Si no existe ningun documento, entonces se inicia con una ID 0, en caso contrario se obtiene la ultima ID y se le suma 1
create(Sn1, Fecha, Nombre, Contenido, Sn2):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getDocumentsP(Sn1, Documentos),
        getLastIdDocument(Documentos, ID),
        (ID = -1),
        ID2 is ID+1,
        document(ID2,OnlineUser,Nombre,Contenido,Fecha,[[0, Contenido]], NuevoDoc),
        getNameP(Sn1, NombreP),
        getDateP(Sn1, FechaP),
        getUsersP(Sn1, Usuarios),
        getAccessesP(Sn1, Accesos),
        append(Documentos, [NuevoDoc], NuevosDocumentos),
        Sn2 = [NombreP, FechaP, Usuarios, NuevosDocumentos, Accesos, ""], !.
create(Sn1, Fecha, Nombre, Contenido, Sn2):-
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
% Busca si existe un permiso en una lista de accesos, si existe lo agrega, sin repetir elementos dentro del mismo
% Si no existe el permiso, crea uno nuevo y lo agrega a la lista de accesos
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
% Comprueba que el usuario sea dueño o pueda compartir y crea un acceso, en caso que no lo sea, retorna falso
% Este acceso se utiliza en buscaryagregar donde se agrega el permiso a la nueva lista de accesos NuevoAccesos
% La salida consiste en una nueva lista de Accesos, con los nuevos permisos
agregar(ID, Documentos, Usuario, Accesos, Permisos, [H|T], ListaOriginal, Salida):-
        (esDuenio(Documentos, ID, Usuario) ; canShare(ID, Usuario, Accesos)), T=[], access(ID, H, Permisos, Acceso), buscaryagregar(Acceso, [], ListaOriginal, NuevoAccesos),
        Salida = NuevoAccesos, !.
agregar(ID, Documentos, Usuario, Accesos, Permisos, [H|T], ListaOriginal, Salida):-
        (esDuenio(Documentos, ID, Usuario) ; canShare(ID, Usuario, Accesos)), access(ID, H, Permisos, Acceso), buscaryagregar(Acceso, [], ListaOriginal, NuevoAccesos),
        agregar(ID, Documentos, Usuario, Accesos, Permisos, T, NuevoAccesos, Salida),!.
% Si no existe un usuario logeado retorna false, lo mismo ocurre si el usuario no puede compartir (No es dueño o no tiene permiso para compartir)
% Si el usuario existe y tiene los permisos, se retorna un paradgimadocs actualizado que incluye los nuevos permisos otorgados a otros usuarios
share(Sn1, DocumentId, ListaPermisos, ListaUsernamesPermitidos, Sn2):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        agregar(DocumentId, Documentos, OnlineUser, Accesos, ListaPermisos, ListaUsernamesPermitidos, Accesos, NuevoAccesos),
        Sn2 = [NombreP, Fecha, Usuarios, Documentos, NuevoAccesos, ""],!.
% Agrega texto a un documento siempre y cuando el usuario sea el dueño o tenga permisos de escritura
% Si el usuario no puede escribir en dicho documento, retorna falso
% En caso contrario, devuelve una lista de documentos actualizados que incluye el nuevo contenido y una version para dicho documento modificado
addTexto(ID, Date, Nombre, NuevoTexto, PrimeraParte, [Primero|T], _, Salida):-
        getIdDoc(Primero, IdDoc), getCreadorDoc(Primero, Creador),
        ID=IdDoc, Nombre=Creador, getTextoDoc(Primero, Texto),
        string_concat(Texto, NuevoTexto, TextoSalida),
        getVersionsDoc(Primero, VersionsDoc), getNombreDoc(Primero, NombreDoc),
        last(VersionsDoc, Last),
        getVersionID(Last, LastID),
        LastIdAux is LastID+1,
        append(VersionsDoc, [[LastIdAux, TextoSalida]], VersionsNuevas),
        document(ID,Creador,NombreDoc,TextoSalida,Date,VersionsNuevas,Aux), append(PrimeraParte, [Aux], Aux2), append(Aux2, T, Salida), !.
addTexto(ID, Date, Nombre, NuevoTexto, PrimeraParte, [Primero|T], Accesos, Salida):-
        getIdDoc(Primero, IdDoc), getCreadorDoc(Primero, Creador), canWrite(ID, Nombre, Accesos, Usuario),
        ID=IdDoc, Usuario\="", getTextoDoc(Primero, Texto),
        string_concat(Texto, NuevoTexto, TextoSalida),
        getVersionsDoc(Primero, VersionsDoc), getNombreDoc(Primero, NombreDoc),
        last(VersionsDoc, Last),
        getVersionID(Last, LastID),
        LastIdAux is LastID+1,
        append(VersionsDoc, [[LastIdAux, TextoSalida]], VersionsNuevas),
        document(ID,Creador,NombreDoc,TextoSalida,Date,VersionsNuevas,Aux), append(PrimeraParte, [Aux], Aux2), append(Aux2, T, Salida), !.
addTexto(ID, Date, Nombre, NuevoTexto, PrimeraParte, [H|T], Accesos, Salida):-
        append(PrimeraParte, [H], Aux), addTexto(ID, Date, Nombre, NuevoTexto, Aux, T, Accesos, Salida), !.
% Si un usuario se encuentra logeado, entonces devuelve un paradigmadocs actualizado con un documento modificado, esto incluye el nuevo texto y una nueva version
% En caso contrario, retorna falso
add(Sn1, DocumentId, Date, ContenidoTexto, Sn2):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        addTexto(DocumentId, Date, OnlineUser, ContenidoTexto, [], Documentos, Accesos, NuevoDocumentos),
        Sn2 = [NombreP, Fecha, Usuarios, NuevoDocumentos, Accesos, ""], !.
% Si el usuario es dueño, restaura una version de un documento especifico, la version pasa a ser el texto actual y el texto anterior pasa a ser una version
% Si el usuario no tiene permisos, devuelve falso
% En caso contrario, devuelve una lista de documentos actualizada con la modificacion realizada.
restoreVersion(ID, IdVersion, PrimeraParte, [Primero|T], _, Nombre, Salida):-
        getIdDoc(Primero, IdDoc), getCreadorDoc(Primero, Creador),
        ID=IdDoc, Nombre=Creador, getTextoDoc(Primero, TextoActual), getVersionsDoc(Primero, Versiones), getVersionByID(IdVersion, Versiones, VersionBuscada),
        getVersionTexto(VersionBuscada, NuevoTexto),
        getVersionsDoc(Primero, VersionsDoc), getNombreDoc(Primero, NombreDoc), getFechaDoc(Primero, Date),
        last(VersionsDoc, Last),
        getVersionID(Last, LastID),
        LastIdAux is LastID+1,
        append(VersionsDoc, [[LastIdAux, TextoActual]], VersionsNuevas),
        document(ID,Creador,NombreDoc,NuevoTexto,Date,VersionsNuevas,Aux), append(PrimeraParte, [Aux], Aux2), append(Aux2, T, Salida), !.
restoreVersion(ID, IdVersion, PrimeraParte, [H|T], Accesos, Nombre, Salida):-
        append(PrimeraParte, [H], Aux), restoreVersion(ID, IdVersion, Aux, T, Accesos, Nombre, Salida).
% Si existe un usuario logeado, se devuelve un paradigmadocs actualizado el cual tiene un documento modificado, dicho documento tiene como texto una version anterior del mismo y, la version inicial como una nueva version
% Si el usuario no esta logeado, retorna falso
restore(Sn1, DocumentId, IdVersion, Sn2):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        restoreVersion(DocumentId, IdVersion, [], Documentos, Accesos, OnlineUser, NuevoDocumentos),
        Sn2 = [NombreP, Fecha, Usuarios, NuevoDocumentos, Accesos, ""], !.
% Obtiene recursivamente todos los usuarios que existen en un paradigmadocs
% Devuelve un texto organizado con todos los usuarios de un paradigmadocs
obtenerUsuarios(Usuarios, Texto, UsuariosSalida):-
        getFirstElement(Usuarios, Primero), getUsername(Primero, Nombre), getPassword(Primero, Clave), getUserCreation(Primero, Fecha), getDia(Fecha, Dia), getMes(Fecha, Mes), getAnio(Fecha, Anio),
        atomics_to_string([Texto, "     Usuario: ", Nombre, "\n     Password: ", Clave, "\n     Fecha de creacion: ", Dia, "-", Mes, "-", Anio, "\n"], TextoNuevo),
        getOthers(Usuarios, NuevosUsuarios),
        obtenerUsuarios(NuevosUsuarios, TextoNuevo, UsuariosSalida).
obtenerUsuarios(Usuarios, Texto, UsuariosSalida):-
        atomics_to_string([Texto, "\n"], TextoSalida), Usuarios = [], UsuariosSalida = TextoSalida, !.
% Obtiene recursivamente todas las versiones que tiene un documento especifico
% Devuelve un texto organizado con todas las versiones de un documento
obtenerVersiones(Versiones, Texto, VersionesSalida):-
        getFirstElement(Versiones, Primero), getVersionID(Primero, ID), getVersionTexto(Primero, Contenido),
        atomics_to_string([Texto, "             ID: ", ID, "\n             Contenido: ", Contenido, "\n"], TextoNuevo),
        getOthers(Versiones, NuevasVersiones),
        obtenerVersiones(NuevasVersiones, TextoNuevo, VersionesSalida).
obtenerVersiones(Versiones, Texto, VersionesSalida):-
        Versiones = [], VersionesSalida = Texto, !.
% Obtiene recursivamente todos los documentos que tiene un paradigmadocs
% Devuelve un texto organizado con la informacion de cada documento, esto incluye sus versiones.
obtenerDocumentos(Documentos, Texto, DocumentosSalida):-
        getFirstElement(Documentos, Primero), getIdDoc(Primero, ID), getCreadorDoc(Primero, Creador), getNombreDoc(Primero, Nombre), getTextoDoc(Primero, Contenido), getFechaDoc(Primero, Fecha), getVersionsDoc(Primero, Versiones),
        getDia(Fecha, Dia), getMes(Fecha, Mes), getAnio(Fecha, Anio), obtenerVersiones(Versiones, "", VersionesSalida),
        atomics_to_string([Texto, "     ID: ", ID, "\n     Creador: ", Creador, "\n     Nombre del documento: ", Nombre, "\n     Contenido: ", Contenido, "\n     Ultima modificacion: ", Dia, "-", Mes, "-", Anio, "\n     Versiones: \n", VersionesSalida, "\n"], TextoNuevo),
        getOthers(Documentos, NuevosDocumentos),
        obtenerDocumentos(NuevosDocumentos, TextoNuevo, DocumentosSalida).
obtenerDocumentos(Documentos, Texto, DocumentosSalida):-
        Documentos = [], DocumentosSalida = Texto,!.
% En base a una lista de permisos, entrega un texto organizado que tiene los tipos de permiso que tiene un usuario
% Devuelve un texto organizado con los permisos que tiene un usuario
obtenerPermisos(Permisos, Texto, PermisosSalida):-
        getFirstElement(Permisos, Primero),
        Primero = "w",
        atomics_to_string([Texto, "             Permiso de escritura (w)\n"], TextoNuevo),
        getOthers(Permisos, NuevosPermisos),
        obtenerPermisos(NuevosPermisos, TextoNuevo, PermisosSalida).
obtenerPermisos(Permisos, Texto, PermisosSalida):-
        getFirstElement(Permisos, Primero),
        Primero = "r",
        atomics_to_string([Texto, "             Permiso de lectura (r)\n"], TextoNuevo),
        getOthers(Permisos, NuevosPermisos),
        obtenerPermisos(NuevosPermisos, TextoNuevo, PermisosSalida).
obtenerPermisos(Permisos, Texto, PermisosSalida):-
        getFirstElement(Permisos, Primero),
        Primero = "c",
        atomics_to_string([Texto, "             Permiso de comentario (c)\n"], TextoNuevo),
        getOthers(Permisos, NuevosPermisos),
        obtenerPermisos(NuevosPermisos, TextoNuevo, PermisosSalida).
obtenerPermisos(Permisos, Texto, PermisosSalida):-
        getFirstElement(Permisos, Primero),
        Primero = "s",
        atomics_to_string([Texto, "             Permiso de compartir (s)\n"], TextoNuevo),
        getOthers(Permisos, NuevosPermisos),
        obtenerPermisos(NuevosPermisos, TextoNuevo, PermisosSalida).
obtenerPermisos(Permisos, Texto, PermisosSalida):-
        getFirstElement(Permisos, Primero),
        Primero \= "w", Primero \= "r", Primero \= "c", Primero \= "s",
        atomics_to_string([Texto, "             Permiso especial (", Primero, ")\n"], TextoNuevo),
        getOthers(Permisos, NuevosPermisos),
        obtenerPermisos(NuevosPermisos, TextoNuevo, PermisosSalida).
obtenerPermisos(Permisos, Texto, PermisosSalida):-
        Permisos = [], PermisosSalida = Texto, !.
% Obtiene todos los accesos de un paradigmadocs
% Devuelve un texto organizado con todos los accesos que tiene cada usuario dentro de un paradigmadocs
obtenerAccesos(Accesos, Texto, AccesosSalida):-
        getFirstElement(Accesos, Primero), getIdDocAccess(Primero, ID), getUsernameAccess(Primero, Nombre), getListAccess(Primero, Lista),
        obtenerPermisos(Lista, "", PermisosSalida),
        atomics_to_string([Texto, "     ID del documento: ", ID, "\n     Usuario: ", Nombre, "\n     Permisos:\n", PermisosSalida, "\n"], TextoNuevo),
        getOthers(Accesos, AccesosNuevos),
        obtenerAccesos(AccesosNuevos, TextoNuevo, AccesosSalida).
obtenerAccesos(Accesos, Texto, AccesosSalida):-
        Accesos = [], AccesosSalida = Texto, !.
% Obtiene toda la informacion de un paradigmadocs
% Devuelve un texto con toda la información de la plataforma, de tal manera que sea legible.
escribirTodo(NombreP, Fecha, Usuarios, Documentos, Accesos, StrOut):-
        getDia(Fecha, Dia), getMes(Fecha, Mes), getAnio(Fecha, Anio), atomics_to_string([Dia, "-", Mes, "-", Anio], FechaSalida),
        obtenerUsuarios(Usuarios, "", UsuariosSalida), obtenerDocumentos(Documentos, "", DocumentosSalida), obtenerAccesos(Accesos, "", AccesosSalida),
        atomics_to_string(["Nombre de la plataforma: ", NombreP, "\nFecha de Creacion: ", FechaSalida, "\nUsuarios:\n", UsuariosSalida, "Documentos:\n", DocumentosSalida, "Accesos:\n", AccesosSalida], StrOut).
% Obtiene toda la informacion de un usuario especifico
% Devuelve un texto organizado con toda la información de un usuario
obtenerUsuarioInfo(Usuarios, Nombre, Salida):-
        getFirstElement(Usuarios, Primero), getUsername(Primero, Usuario), Nombre=Usuario,
        getPassword(Primero, Clave), getUserCreation(Primero, Fecha), getDia(Fecha, Dia), getMes(Fecha, Mes), getAnio(Fecha, Anio),
        atomics_to_string(["     Usuario: ", Nombre, "\n     Password: ", Clave, "\n     Fecha de creacion: ", Dia, "-", Mes, "-", Anio, "\n"], Salida), !.
obtenerUsuarioInfo(Usuarios, Nombre, Salida):-
        getOthers(Usuarios, NuevosUsuarios), obtenerUsuarioInfo(NuevosUsuarios, Nombre, Salida).
% Obtiene toda la informacion de los documentos de un usuario especifico
% Devuelve un texto organizado con toda la información de los documentos de un usuario
obtenerDocumentosUsuario(Documentos, Nombre, Texto, Salida):-
        getFirstElement(Documentos, Primero), getCreadorDoc(Primero, Creador), Nombre=Creador,
        getIdDoc(Primero, ID), getNombreDoc(Primero, NombreDoc), getTextoDoc(Primero, Contenido), getFechaDoc(Primero, Fecha), getVersionsDoc(Primero, Versiones),
        getDia(Fecha, Dia), getMes(Fecha, Mes), getAnio(Fecha, Anio), obtenerVersiones(Versiones, "", VersionesSalida),
        atomics_to_string([Texto, "     ID: ", ID, "\n     Creador: ", Nombre, "\n     Nombre del documento: ", NombreDoc, "\n     Contenido: ", Contenido, "\n     Ultima modificacion: ", Dia, "-", Mes, "-", Anio, "\n     Versiones: \n", VersionesSalida, "\n"], TextoNuevo),
        getOthers(Documentos, NuevosDocumentos),
        obtenerDocumentosUsuario(NuevosDocumentos, Nombre, TextoNuevo, Salida), !.
obtenerDocumentosUsuario(Documentos, Nombre, Texto, Salida):-
        getOthers(Documentos, NuevosDocumentos),
        obtenerDocumentosUsuario(NuevosDocumentos, Nombre, Texto, Salida), !.
obtenerDocumentosUsuario(Documentos, _, Texto, Salida):-
        Documentos = [], Salida = Texto, !.
% Obtiene toda la informacion de los accesos de un usuario especifico
% Devuelve un texto organizado con toda la información de los accesos de un usuario
obtenerAccesosUsuario(Accesos, Nombre, Texto, Salida):-
        getFirstElement(Accesos, Primero), getUsernameAccess(Primero, Usuario), Nombre=Usuario, getIdDocAccess(Primero, ID), getListAccess(Primero, Lista),
        obtenerPermisos(Lista, "", PermisosSalida),
        atomics_to_string([Texto, "     ID del documento: ", ID, "\n     Usuario: ", Nombre, "\n     Permisos:\n", PermisosSalida, "\n"], TextoNuevo),
        getOthers(Accesos, AccesosNuevos),
        obtenerAccesosUsuario(AccesosNuevos, Nombre, TextoNuevo, Salida), !.
obtenerAccesosUsuario(Accesos, Nombre, Texto, Salida):-
        getOthers(Accesos, NuevosAccesos),
        obtenerAccesosUsuario(NuevosAccesos, Nombre, Texto, Salida), !.
obtenerAccesosUsuario(Accesos, _, Texto, Salida):-
        Accesos = [], Salida = Texto, !.
% Obtiene toda la informacion relevante de un usuario especifico dentro de un paradigmadocs
% Devuelve un texto organizado con toda la informacion relevante de un usuario
escribirUsuario(Usuario, NombreP, Fecha, Usuarios, Documentos, Accesos, StrOut):-
        getDia(Fecha, Dia), getMes(Fecha, Mes), getAnio(Fecha, Anio), atomics_to_string([Dia, "-", Mes, "-", Anio], FechaSalida),
        obtenerUsuarioInfo(Usuarios, Usuario, UsuarioSalida), obtenerDocumentosUsuario(Documentos, Usuario, "", DocumentosSalida), obtenerAccesosUsuario(Accesos, Usuario, "", AccesosSalida),
        atomics_to_string(["Nombre de la plataforma: ", NombreP, "\nFecha de Creacion: ", FechaSalida, "\nUsuarios:\n", UsuarioSalida, "Documentos:\n", DocumentosSalida, "Accesos:\n", AccesosSalida], StrOut).
% Comprueba que exista un usuario logeado, si es asi devuelve un texto organizado con toda la informacion relevante de dicho usuario perteneciente a un paradigmadocs
% En caso contrario, devuelve un texto organizado con toda la información de un paradigmadocs.
toString(Sn1, StrOut):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser = "",
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        escribirTodo(NombreP, Fecha, Usuarios, Documentos, Accesos, StrOut), !.
toString(Sn1, StrOut):-
        getOnlineUserP(Sn1, OnlineUser),
        OnlineUser \= "",
        getNameP(Sn1, NombreP),
        getDateP(Sn1, Fecha),
        getUsersP(Sn1, Usuarios),
        getDocumentsP(Sn1, Documentos),
        getAccessesP(Sn1, Accesos),
        escribirUsuario(OnlineUser, NombreP, Fecha, Usuarios, Documentos, Accesos, StrOut), !.
/*
                Requerimientos funcionales.
% Dominios:
        Sn1: paradigmadocs
        Fecha: date
        Username: string
        Password: string
        Sn2: paradigmadocs
        Nombre: string
        Contenido: string
        DocumentId: entero
        ListaPermisos: string list
        ListaUsernamesPermitidos: string list
        Date: Date
        ContenidoTexto: string
        IdVersion: entero
        StrOut: string
% Predicados y Metas:
        Predicados con metas principales:
                paradigmaDocsRegister(Sn1, Fecha, Username, Password, Sn2).
                paradigmaDocsLogin(Sn1, Username, Password, Sn2).
                paradigmaDocsCreate(Sn1, Fecha, Nombre, Contenido, Sn2).
                paradigmaDocsShare(Sn1, DocumentId, ListaPermisos, ListaUsernamesPermitidos, Sn2).
                paradigmaDocsAdd(Sn1, DocumentId, Date, ContenidoTexto, Sn2).
                paradigmaDocsRestoreVersion(Sn1, DocumentId, IdVersion, Sn2).
                paradigmaDocsToString(Sn1, StrOut).
                paradigmaDocsToString(Sn1, StrOut).
*/
%               Clausulas
% registra a un usuario en un paradigmadocs, si el usuario ya existe retorna falso, en caso contrario se obtiene una version actualizada de paradigmadocs con el usuario nuevo
paradigmaDocsRegister(Sn1, Fecha, Username, Password, Sn2):-
        register(Sn1, Fecha, Username, Password, Sn2), !.
% logea a un usuario en un paradigmadocs, si el usuario no existe retorna falso, en caso contrario se obtiene una version actualizada de paradigmadocs con el usuario activo
paradigmaDocsLogin(Sn1, Username, Password, Sn2):-
        login(Sn1, Username, Password, Sn2), !.
% Si no existe un usuario logeado o el usuario no tiene permisos para escribir (es dueño o tiene permiso de escritura) retorna falso, en caso contrario
% retorna un paradigmadocs actualizado con un nuevo documento
paradigmaDocsCreate(Sn1, Fecha, Nombre, Contenido, Sn2):-
        create(Sn1, Fecha, Nombre, Contenido, Sn2), !.
% Si no existe un usuario logeado o el usuario no tiene permisos para compartir (es dueño o tiene permiso de compartir) retorna falso, en caso contrario
% retorna un paradigmadocs actualizado con nuevos accesos en la lista de accesos de paradigmadocs
paradigmaDocsShare(Sn1, DocumentId, ListaPermisos, ListaUsernamesPermitidos, Sn2):-
        share(Sn1, DocumentId, ListaPermisos, ListaUsernamesPermitidos, Sn2), !.
% Si no existe un usuario logeado o el usuario no tiene permisos para escribir (es dueño o tiene permiso de escritura) retorna falso, en caso contrario
% retorna un paradigmadocs actualizado con un documento especifico actualizado (nuevo texto actual, nueva version)
paradigmaDocsAdd(Sn1, DocumentId, Date, ContenidoTexto, Sn2):-
        add(Sn1, DocumentId, Date, ContenidoTexto, Sn2), !.
% Si no existe un usuario logeado o el usuario no tiene permisos para escribir (es dueño o tiene permiso de escritura) retorna falso, en caso contrario
% retorna un paradigmadocs actualizado con un documento especifico actualizado (version antigua pasa ser actual, version anterior pasa a ser una version)
paradigmaDocsRestoreVersion(Sn1, DocumentId, IdVersion, Sn2):-
        restore(Sn1, DocumentId, IdVersion, Sn2), !.
% Si existe un usuario logeado, entrega la informacion correspondiente a este en un string
% En caso contrario, entrega la informacion de toda la plataforma en un string
paradigmaDocsToString(Sn1, StrOut):-
        toString(Sn1, StrOut), !.

% Ejemplos
/*
                paradigmaDocs
date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1).
date([3,2,2021], Fecha), paradigmaDocs("Plataforma2", Fecha, P2).
date([1,12,2021], Fecha), paradigmaDocs("Plataforma3", Fecha, P3).
                paradigmaDocsRegister
date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4).

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Juan", "Clave3", P4). % Esta ejecucion de los predicados retorna falso, ya que el usuario Juan ya existe.

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave", P4). % Los usuarios pueden tener la misma clave.
                paradigmaDocsLogin
date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5). % Queda Juan como usuario activo

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave", P5). % Esta ejecucion de los predicados retorna falso, ya que la clave es incorrecta.

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Matias", "Clave", P5). % Esta ejecucion de los predicados retorna falso, ya que el usuario no existe.
                paradigmaDocsCreate
date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10). % 2 archivos para Juan y uno para Diego

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsCreate(P4, Fecha, "Doc1", "Contenido del primer archivo", P5). % Esta ejecucion falla ya que no se logea ningun usuario

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
date(30,2,2021, Fecha2), paradigmaDocsCreate(P5, Fecha2, "Doc1", "Contenido del primer archivo", P6). % Esta ejecucion falla ya que la fecha ingresada no existe.
                paradigmaDocsShare
date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "t"], ["Diego", "Pedro"], P12). % Diego y Pedro tendran permisos de escritura y permiso especial "t" en el documento 1 de Juan.

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 2, ["w", "t"], ["Diego", "Pedro"], P12). % Retorna falso ya que Juan no tiene permiso de compartir el documento 2

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "s"], ["Diego"], P12),
paradigmaDocsLogin(P12, "Diego", "Clave3", P13),
paradigmaDocsShare(P13, 1, ["w", "c"], ["Pedro"], P14). % Ahora Diego tiene permisos de compartir y da los permisos de escritura y comentar a Pedro en el documento de Juan.
                paradigmaDocsAdd
date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "s"], ["Diego"], P12),
paradigmaDocsLogin(P12, "Diego", "Clave3", P13),
paradigmaDocsShare(P13, 1, ["w", "c"], ["Pedro"], P14),
paradigmaDocsLogin(P14, "Juan", "Clave2", P15),
date(2 ,2,2021, Fecha2), paradigmaDocsAdd(P15, 1, Fecha2, ", que ha sido modificado por primera vez", P16),
paradigmaDocsLogin(P16, "Juan", "Clave2", P17),
paradigmaDocsAdd(P17, 1, Fecha2, ", que ha sido modificado por segunda vez", P18). % Juan modifica dos veces el archivo 1

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "s"], ["Diego"], P12),
paradigmaDocsLogin(P12, "Diego", "Clave3", P13),
paradigmaDocsShare(P13, 1, ["w", "c"], ["Pedro"], P14),
paradigmaDocsLogin(P14, "Diego", "Clave3", P15),
date(2 ,2,2021, Fecha2), paradigmaDocsAdd(P15, 1, Fecha2, ", que ha sido modificado por primera vez", P16). % Diego, que tiene permiso de escritura modifica el archivo de Juan

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "s"], ["Diego"], P12),
paradigmaDocsLogin(P12, "Pedro", "Clave", P13),
date(2 ,2,2021, Fecha2), paradigmaDocsAdd(P13, 1, Fecha2, ", que ha sido modificado por primera vez", P14). % False, en este momento Pedro no tiene permisos de escritura en el documento 1
                paradigmaDocsRestoreVersion
date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "s"], ["Diego"], P12),
paradigmaDocsLogin(P12, "Diego", "Clave3", P13),
paradigmaDocsShare(P13, 1, ["w", "c"], ["Pedro"], P14),
paradigmaDocsLogin(P14, "Juan", "Clave2", P15),
date(2 ,2,2021, Fecha2), paradigmaDocsAdd(P15, 1, Fecha2, ", que ha sido modificado por primera vez", P16),
paradigmaDocsLogin(P16, "Juan", "Clave2", P17),
paradigmaDocsAdd(P17, 1, Fecha2, ", que ha sido modificado por segunda vez", P18),
paradigmaDocsLogin(P18, "Juan", "Clave2", P19),
paradigmaDocsRestoreVersion(P19, 1, 1, P20). % Juan, que es el dueño restaura la version 1

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "s"], ["Diego"], P12),
paradigmaDocsLogin(P12, "Diego", "Clave3", P13),
paradigmaDocsShare(P13, 1, ["w", "c"], ["Pedro"], P14),
paradigmaDocsLogin(P14, "Diego", "Clave3", P15),
date(2 ,2,2021, Fecha2), paradigmaDocsAdd(P15, 1, Fecha2, ", que ha sido modificado por primera vez", P16),
paradigmaDocsLogin(P16, "Diego", "Clave3", P17),
paradigmaDocsRestoreVersion(P17, 1, 0, P18). % Diego, que tiene permiso de escritura no puede restaurar el documento

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "s"], ["Diego"], P12),
paradigmaDocsLogin(P12, "Pedro", "Clave", P13),
paradigmaDocsRestoreVersion(P13, 1, 0, P14). % Falso, Pedro no tiene ningun tipo de permiso
                paradigmaDocsToString
date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1), paradigmaDocsToString(P1, Salida), write(Salida). % Se entrega la informacion de una plataforma vacia, se utiliza write para mostrarlo mas facilmente, no es necesario usarlo.

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "s"], ["Diego"], P12),
paradigmaDocsLogin(P12, "Diego", "Clave3", P13),
paradigmaDocsShare(P13, 1, ["w", "c"], ["Pedro"], P14),
paradigmaDocsLogin(P14, "Juan", "Clave2", P15),
date(2 ,2,2021, Fecha2), paradigmaDocsAdd(P15, 1, Fecha2, ", que ha sido modificado por primera vez", P16),
paradigmaDocsLogin(P16, "Juan", "Clave2", P17),
paradigmaDocsAdd(P17, 1, Fecha2, ", que ha sido modificado por segunda vez", P18),
paradigmaDocsLogin(P18, "Juan", "Clave2", P19),
paradigmaDocsRestoreVersion(P19, 1, 1, P20),
paradigmaDocsToString(P20, Salida), write(Salida). % Se imprime una plataforma completa, nuevamente se utiliza write para mostrar todo el texto de manera ordenada

date(1,2,2021, Fecha), paradigmaDocs("Plataforma1", Fecha, P1),
paradigmaDocsRegister(P1, Fecha, "Pedro", "Clave1", P2),
paradigmaDocsRegister(P2, Fecha, "Juan", "Clave2", P3),
paradigmaDocsRegister(P3, Fecha, "Diego", "Clave3", P4),
paradigmaDocsLogin(P4, "Juan", "Clave2", P5),
paradigmaDocsCreate(P5, Fecha, "Doc1", "Contenido del primer archivo", P6),
paradigmaDocsLogin(P6, "Juan", "Clave2", P7),
paradigmaDocsCreate(P7, Fecha, "Doc2", "Contenido del segundo archivo", P8),
paradigmaDocsLogin(P8, "Diego", "Clave3", P9),
paradigmaDocsCreate(P9, Fecha, "Doc3", "Contenido del tercer archivo", P10),
paradigmaDocsLogin(P10, "Juan", "Clave2", P11),
paradigmaDocsShare(P11, 1, ["w", "s"], ["Diego"], P12),
paradigmaDocsLogin(P12, "Diego", "Clave3", P13),
paradigmaDocsShare(P13, 1, ["w", "c"], ["Pedro"], P14),
paradigmaDocsLogin(P14, "Juan", "Clave2", P15),
date(2 ,2,2021, Fecha2), paradigmaDocsAdd(P15, 1, Fecha2, ", que ha sido modificado por primera vez", P16),
paradigmaDocsLogin(P16, "Juan", "Clave2", P17),
paradigmaDocsAdd(P17, 1, Fecha2, ", que ha sido modificado por segunda vez", P18),
paradigmaDocsLogin(P18, "Juan", "Clave2", P19),
paradigmaDocsRestoreVersion(P19, 1, 1, P20),
paradigmaDocsLogin(P20, "Juan", "Clave2", P21),
paradigmaDocsToString(P21, Salida), write(Salida). % Se imprime la informacion de la plataforma y Juan, se utiliza write para mostrar nuevamente de forma mas clara.
*/
