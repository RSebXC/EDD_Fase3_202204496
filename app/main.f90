program main
    use json_module
    use hash_table
    use Avl_Tree
    use routes
    use PasswordEncryptor
    IMPLICIT NONE
    type(HashTable) :: table
    integer, parameter :: long = selected_int_kind(4)
    character(len=200) :: filename_json
    character(len=20) :: loginUser, loginPass
    character(:), allocatable :: name, lastname, genre
    character(:), allocatable :: address, departamento, direccion, password
    integer ::  i, size, id_sucursal, s1, s2, distancia, imp_mantenimiento, rutaDestino, costoTotal, ingresoTotal, &
    costoTotal2,ingresoTotal2
    integer :: ganancia1, ganancia2, opcion
    integer :: contador = 0
    integer*8 ::  dpi, phone
    character(:), allocatable ::  dpi_value, phone_value
    type(json_value), pointer :: tech_pointer, personPointer, attributePointer, sucur_pointer, deptPointer, partPointer
    type(json_value), pointer :: edgepointer, grafo_pointer
    type(json_file) :: json
    type(json_core) :: jsonc
    logical :: found
    type(Tree_t) :: avl_sucursales
    type(graph) :: graphRutas, graphImp
    type(analyzer):: analizador, analizadorImp
    type(result_list) :: listaRuta, listabyImp
    character(:), allocatable :: Encriptada
    character(len=30) :: passwordEncriptada
    
  
    print *,"Iniciar Sesion"
    print *, "Ingrese su usuario:"
    read (*, '(A)') loginUser
    print *, "Ingrese su contrasena"
    read (*, '(A)') loginPass
  
    if (loginUser == "admin" .and. loginPass == "123") then
      do
        print *, "1. Cargar archivos"
        print *, "2. Administrar las Sucursales"
        print *, "3. Generar los Reportes"
        print *, "4. Salir"
        print *, "----------------------------------------------------"
        print *, "Ingrese una opcion: "
        read(*,*) opcion
        select case(opcion)
        case(1)
          print *, "1.1 Sucursales"
          print *, "1.2 Rutas"
          print *, "Ingrese una opcion:"
          read(*,*) opcion
      
          select case(opcion)
            case (1)
              print *, "Carga de sucursales."
              print *, "Ingrese la ruta del archivo de Sucursales: "
              read (*,*) filename_json
              call leerJson()
      
              case (2)
                print *, "Carga de rutas."
                print *, "Ingrese la ruta del archivo de Rutas: "
                read (*,*) filename_json
                call leerJson2()
                
              case default
                  print *,  "Opcion invalida."
          end select
        case(2)
          print *, "Sucursales seleccionado."
          print *, "Ingrese el id de la sucursal: "
          read (*,*) id_sucursal
          print *, "Ingrese la contrasena de la sucursal: "
          read (*,*) passwordEncriptada
          Encriptada = hashPassword(trim(passwordEncriptada))
          call avl_sucursales%searchNode(id_sucursal, Encriptada)
          print *, "Bienvenido a la sucursal ", id_sucursal
          print *, "Ingrese una opcion: "
          print *, "1. Cargar tecnicos"
          print *, "2. Generar el recorrido más optimo"
          print *, "3. Informacion Tecnico en Especifico"
          print *, "4. Listar Tecnicos"
          print *, "5. Generar Reportes"
          print *, "6. Regresar"
          read (*,*) opcion
          select case(opcion)
            case(1)
              ! Código para la opción "Carga de tecnicos"
              print *, "Ingrese la ruta del archivo de tecnicos: "
              read (*,*) filename_json
              call leerJson3()
              
  
            case(2)
              print *, "Ingresa un DPI"
              read(*,*) dpi
              call avl_sucursales%searchNode_tecnico_trabajo(id_sucursal, dpi)
              print *, "Ingrese la ruta de destino"
              read (*,*) rutaDestino
              listaRuta = analizador%get_shortest_path(id_sucursal,rutaDestino)
              listabyImp = analizadorImp%get_longest_path(id_sucursal,rutaDestino)
              call listaRuta%print()
              print *, "Opcion 1: "
              costoTotal = 80 * listaRuta%total_weight
              ingresoTotal = 100 * listaRuta%total_printers
              print *, "Impresoras: ", listaRuta%total_printers
              ganancia1 = ingresoTotal - costoTotal
              print *, "--------------"
              call listabyImp%print()
              print *, "Opcion 2: "
              costoTotal2 = 80 * listabyImp%total_printers
              ingresoTotal2 = 100 * listabyImp%total_weight
              print *, "Distancia: ",listabyImp%total_printers
              ganancia2 = ingresoTotal2 - costoTotal2
              
              if (ganancia1 > ganancia2) then 
                print *, "La mejor solucion es la opcion 1 con una ganacia de ",ingresoTotal
                print *, "Costo Total: ", costoTotal
                contador = contador + ingresoTotal
              else if (ganancia2>ganancia1)then
                print *, "La mejor solucion es la opcion 2 con una ganacia de ",ingresoTotal2
                print *, "Costo Total: ", costoTotal2
                contador = contador + ingresoTotal2

              else 
                print *, "Ambas opciones tienen la misma ganancia", ingresoTotal
                print *, "Costo Total: ", costoTotal
                contador = contador + ingresoTotal

              end if
            case(3)
              print *, "Ingrese el DPI del tecnico: "
              read (*,*) dpi
              call avl_sucursales%searchNodeESPE(id_sucursal, dpi)
            case(4)
              print *, "Listar Tecnicos"
              call avl_sucursales%searchNodeLIST(id_sucursal)
            case(5)
              print *, "Top de Tecnicos"
              call avl_sucursales%searchNodeTOP(id_sucursal)
            end select

          case(3)
            print *, "Reporte de Sucursales"
            call avl_sucursales%printTopFiveMaxValues()
            print *, "Ganancia Total: ", contador
          case(4)
            exit
      end select
        
      end do
    end if

    contains

    subroutine leerJson()
      call json%initialize()
              call json%load(filename=filename_json)
              call json%info('', n_children=size)
              call json%get_core(jsonc)
              call json%get('', sucur_pointer, found)
              do i = 1, size
                call jsonc%get_child(sucur_pointer, i, deptPointer, found=found)
                  call jsonc%get_child(deptPointer, 'id', partPointer, found=found)
                  if (found) then
                    call jsonc%get(partPointer, id_sucursal)
                    print *, 'ID: ', id_sucursal
                  end if
                          
                  call jsonc%get_child(deptPointer, 'departamento', partPointer, found=found)
                  if (found) then
                    call jsonc%get(partPointer, departamento)
                    print *, 'Departamento: ', departamento
                  end if
                          
                  call jsonc%get_child(deptPointer, 'direccion', partPointer, found=found)
                  if (found) then
                    call jsonc%get(partPointer, direccion)
                    print *, 'Direccion: ', direccion
                  end if
                  call jsonc%get_child(deptPointer, 'password', partPointer, found=found)
                  if (found) then
                    call jsonc%get(partPointer, password)
                    print *, 'Contrasena: ', password
                  end if
                  Encriptada = hashPassword(password)
                  call avl_sucursales%insert_node(id_sucursal, departamento, direccion, Encriptada)
              end do
              call avl_sucursales%graph_avl_tree()
              call json%destroy()
    end subroutine leerJson

    subroutine leerJson2()
      ! Inicializar y cargar el JSON desde el archivo
      call json%initialize()
      call json%load(filename=filename_json)
      call json%info('grafo', n_children=size) ! Aquí especificamos 'grafo' como la clave del arreglo
      call json%get('grafo', grafo_pointer, found) ! Obtenemos el puntero al arreglo 'grafo'

      do i = 1, size
          call jsonc%get_child(grafo_pointer, i, edgePointer, found=found) ! Obtenemos el i-ésimo elemento del arreglo 'grafo'

          call jsonc%get_child(edgePointer, 's1', partPointer, found=found)
          if (found) then
              call jsonc%get(partPointer, s1)
          end if

          call jsonc%get_child(edgePointer, 's2', partPointer, found=found)
          if (found) then
              call jsonc%get(partPointer, s2)
          end if

          call jsonc%get_child(edgePointer, 'distancia', partPointer, found=found)
          if (found) then
              call jsonc%get(partPointer, distancia)
          end if

          call jsonc%get_child(edgePointer, 'imp_mantenimiento', partPointer, found=found)
          if (found) then
              call jsonc%get(partPointer, imp_mantenimiento)
          end if
          call graphRutas%insert_data(s1,s2,distancia,imp_mantenimiento)
          call graphImp%insert_data_2(s1,s2,imp_mantenimiento, distancia)
      end do
      print *, " RUTAS CARGADAS---------------------"
      call graphRutas%show()
      call analizador%set_graph(graphRutas)
      call analizadorImp%set_graph(graphImp)
      call graphRutas%graficar('Rute')
      call json%destroy()
    end subroutine leerJson2

    subroutine leerJson3()
      call json%initialize()    ! Se inicializa el módulo JSON
              call json%load(filename=filename_json)  ! Se carga el archivo JSON llamado 'config.json'
              call json%print()         ! Se imprime el contenido del archivo JSON (opcional)
              call json%info('',n_children=size)
              call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
              call json%get('', tech_pointer, found)
              do i=1, size
                  call jsonc%get_child(tech_pointer, i, personPointer, found=found)
                  call jsonc%get_child(personPointer, 'dpi', attributePointer, found=found)
                  if (found) then
                      call jsonc%get(attributePointer, dpi_value)
                      read(dpi_value,'(I13)') dpi 
                  end if
                  call jsonc%get_child(personPointer, 'nombre', attributePointer, found=found)
                  if (found) then
                      call jsonc%get(attributePointer, name)
                  end if
                  call jsonc%get_child(personPointer, 'apellido', attributePointer, found=found)
                  if (found) then
                      call jsonc%get(attributePointer, lastname)
                  end if
                  call jsonc%get_child(personPointer, 'genero', attributePointer, found=found)
                  if (found) then
                      call jsonc%get(attributePointer, genre)
                  end if
                  call jsonc%get_child(personPointer, 'direccion', attributePointer, found=found)
                  if (found) then
                      call jsonc%get(attributePointer, address)
                  end if
                  call jsonc%get_child(personPointer, 'telefono', attributePointer, found=found)
                  if (found) then
                      call jsonc%get(attributePointer, phone_value)
                      read(phone_value,'(I13)') phone
                  end if
                  call table%insert(dpi, name, lastname, address, phone)
              end do
                  call table%print()
                  call avl_sucursales%searchNode_hash(id_sucursal, table)
                  call table%grafico("hash.dot")
  
                  call json%destroy()
      end subroutine leerJson3

  end program main
  