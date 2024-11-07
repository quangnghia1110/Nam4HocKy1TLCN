package studentConsulting.model.payload.mapper.admin;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import studentConsulting.model.entity.WardEntity;
import studentConsulting.model.payload.dto.manage.ManageWardDTO;

@Mapper(componentModel = "spring")
public interface WardMapper {

    @Mapping(source = "code", target = "code")
    @Mapping(source = "name", target = "name")
    @Mapping(source = "nameEn", target = "nameEn")
    @Mapping(source = "fullName", target = "fullName")
    @Mapping(source = "fullNameEn", target = "fullNameEn")
    @Mapping(source = "codeName", target = "codeName")
    @Mapping(source = "district.code", target = "districtCode")
    ManageWardDTO mapToDTO(WardEntity ward);
}
