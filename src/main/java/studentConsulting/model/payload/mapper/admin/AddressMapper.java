package studentConsulting.model.payload.mapper.admin;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import studentConsulting.model.entity.AddressEntity;
import studentConsulting.model.payload.dto.manage.ManageAddressDTO;

@Mapper(componentModel = "spring")
public interface AddressMapper {

    @Mapping(source = "line", target = "line")
    @Mapping(source = "province.code", target = "provinceCode")
    @Mapping(source = "district.code", target = "districtCode")
    @Mapping(source = "ward.code", target = "wardCode")
    ManageAddressDTO mapToDTO(AddressEntity address);
}
