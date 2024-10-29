package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.address.DistrictEntity;
import studentConsulting.model.payload.dto.address.ManageDistrictDTO;

@Component
public class DistrictMapper {
    public ManageDistrictDTO mapToDTO(DistrictEntity district) {
        return ManageDistrictDTO.builder()
                .code(district.getCode())
                .name(district.getName())
                .nameEn(district.getNameEn())
                .fullName(district.getFullName())
                .fullNameEn(district.getFullNameEn())
                .codeName(district.getCodeName())
                .provinceCode(district.getProvince() != null ? district.getProvince().getCode() : null)
                .build();
    }
}
