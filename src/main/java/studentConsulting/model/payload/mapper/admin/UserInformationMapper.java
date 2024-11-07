package studentConsulting.model.payload.mapper.admin;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.ConsultantDTO;
import studentConsulting.model.payload.dto.manage.ManageUserDTO;

@Mapper(componentModel = "spring")
public interface UserInformationMapper {

    @Mapping(source = "id", target = "id")
    @Mapping(source = "avatarUrl", target = "avatarUrl")
    @Mapping(source = "createdAt", target = "createdAt")
    @Mapping(source = "firstName", target = "firstName")
    @Mapping(source = "lastName", target = "lastName")
    @Mapping(source = "gender", target = "gender")
    @Mapping(source = "phone", target = "phone")
    @Mapping(source = "schoolName", target = "schoolName")
    @Mapping(source = "studentCode", target = "studentCode")
    @Mapping(target = "address.line", source = "address.line")
    @Mapping(target = "address.provinceFullName", source = "address.province.fullName")
    @Mapping(target = "address.districtFullName", source = "address.district.fullName")
    @Mapping(target = "address.wardFullName", source = "address.ward.fullName")
    ManageUserDTO mapToDTO(UserInformationEntity userInformationEntity);

    @Mapping(source = "account.id", target = "id")
    @Mapping(source = "firstName", target = "firstName")
    @Mapping(source = "lastName", target = "lastName")
    @Mapping(source = "account.email", target = "email")
    @Mapping(source = "phone", target = "phone")
    @Mapping(source = "avatarUrl", target = "avatarUrl")
    @Mapping(target = "department.id", source = "account.department.id")
    @Mapping(target = "department.name", source = "account.department.name")
    ConsultantDTO mapDTO(UserInformationEntity userInfo);
}
