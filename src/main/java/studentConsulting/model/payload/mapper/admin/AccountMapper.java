package studentConsulting.model.payload.mapper.admin;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.mapstruct.NullValuePropertyMappingStrategy;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.entity.DepartmentEntity;
import studentConsulting.model.entity.RoleConsultantEntity;
import studentConsulting.model.entity.RoleEntity;
import studentConsulting.model.payload.dto.manage.ManageAccountDTO;
import studentConsulting.model.payload.dto.manage.ManageActivityDTO;

@Mapper(componentModel = "spring", nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
public interface AccountMapper {

    @Mapping(source = "id", target = "id")
    @Mapping(source = "createdAt", target = "createdAt")
    @Mapping(source = "email", target = "email")
    @Mapping(source = "activity", target = "isActivity")
    @Mapping(source = "username", target = "username")
    @Mapping(source = "lastActivity", target = "lastActivity")
    @Mapping(source = "isOnline", target = "isOnline")
    @Mapping(source = "department", target = "department", qualifiedByName = "mapDepartment")
    @Mapping(source = "role", target = "role", qualifiedByName = "mapRole")
    @Mapping(source = "roleConsultant", target = "roleConsultant", qualifiedByName = "mapRoleConsultant")
    ManageAccountDTO mapToDTO(AccountEntity accountEntity);

    @Named("mapDepartment")
    default ManageAccountDTO.DepartmentDTO mapDepartment(DepartmentEntity department) {
        if (department == null) return null;
        return ManageAccountDTO.DepartmentDTO.builder()
                .id(department.getId())
                .name(department.getName())
                .build();
    }

    @Named("mapRole")
    default ManageAccountDTO.RoleDTO mapRole(RoleEntity role) {
        if (role == null) return null;
        return ManageAccountDTO.RoleDTO.builder()
                .id(role.getId())
                .name(role.getName())
                .build();
    }

    @Named("mapRoleConsultant")
    default ManageAccountDTO.RoleConsultantDTO mapRoleConsultant(RoleConsultantEntity roleConsultant) {
        if (roleConsultant == null) return null;
        return ManageAccountDTO.RoleConsultantDTO.builder()
                .id(roleConsultant.getId())
                .name(roleConsultant.getName())
                .build();
    }


    @Mapping(source = "id", target = "id")
    @Mapping(source = "email", target = "email")
    @Mapping(source = "activity", target = "isActivity")
    ManageActivityDTO mapToDTOs(AccountEntity accountEntity);

}
