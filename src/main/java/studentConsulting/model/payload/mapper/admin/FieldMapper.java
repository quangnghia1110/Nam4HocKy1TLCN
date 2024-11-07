package studentConsulting.model.payload.mapper.admin;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import studentConsulting.model.entity.FieldEntity;
import studentConsulting.model.payload.dto.manage.ManageFieldDTO;

@Mapper(componentModel = "spring")
public interface FieldMapper {

    @Mapping(source = "id", target = "id")
    @Mapping(source = "createdAt", target = "createdAt")
    @Mapping(source = "name", target = "name")
    @Mapping(source = "department.id", target = "departmentId")
    ManageFieldDTO mapToDTO(FieldEntity field);
}
