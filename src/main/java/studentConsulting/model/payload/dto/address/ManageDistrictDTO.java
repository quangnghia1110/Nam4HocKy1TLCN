package studentConsulting.model.payload.dto.address;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ManageDistrictDTO {
    private String code;
    private String name;
    private String nameEn;
    private String fullName;
    private String fullNameEn;
    private String codeName;
    private String provinceCode;
}
