package studentConsulting.model.payload.mapper.admin;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import studentConsulting.model.entity.RoleConsultantEntity;
import studentConsulting.model.payload.dto.manage.ManageRoleConsultantDTO;

@Mapper(componentModel = "spring")
public interface RoleConsultantMapper {

    @Mapping(source = "id", target = "id")
    @Mapping(source = "createdAt", target = "createdAt")
    @Mapping(source = "name", target = "name")
    @Mapping(source = "role.id", target = "roleId")
    ManageRoleConsultantDTO mapToDTO(RoleConsultantEntity roleConsultant);
}
