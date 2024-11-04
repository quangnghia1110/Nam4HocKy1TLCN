package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.WardEntity;
import studentConsulting.model.payload.dto.manage.ManageWardDTO;

@Component
public class WardMapper {
    public ManageWardDTO mapToDTO(WardEntity ward) {
        return ManageWardDTO.builder()
                .code(ward.getCode())
                .name(ward.getName())
                .nameEn(ward.getNameEn())
                .fullName(ward.getFullName())
                .fullNameEn(ward.getFullNameEn())
                .codeName(ward.getCodeName())
                .districtCode(ward.getDistrict() != null ? ward.getDistrict().getCode() : null)
                .build();
    }
}
