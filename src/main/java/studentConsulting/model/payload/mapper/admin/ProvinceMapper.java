package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.address.ProvinceEntity;
import studentConsulting.model.payload.dto.address.ManageProvinceDTO;

@Component
public class ProvinceMapper {
    public ManageProvinceDTO mapToDTO(ProvinceEntity province) {
        return ManageProvinceDTO.builder()
                .code(province.getCode())
                .name(province.getName())
                .nameEn(province.getNameEn())
                .fullName(province.getFullName())
                .fullNameEn(province.getFullNameEn())
                .codeName(province.getCodeName())
                .build();
    }
}
