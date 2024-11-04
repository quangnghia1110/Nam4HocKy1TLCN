package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.AddressEntity;
import studentConsulting.model.payload.dto.manage.ManageAddressDTO;

@Component
public class AddressMapper {

    public ManageAddressDTO mapToDTO(AddressEntity address) {
        return ManageAddressDTO.builder()
                .line(address.getLine())
                .provinceCode(address.getProvince() != null ? address.getProvince().getCode() : null)
                .districtCode(address.getDistrict() != null ? address.getDistrict().getCode() : null)
                .wardCode(address.getWard() != null ? address.getWard().getCode() : null)
                .build();
    }
}
