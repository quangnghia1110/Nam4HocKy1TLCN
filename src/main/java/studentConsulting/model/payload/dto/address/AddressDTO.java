package studentConsulting.model.payload.dto.address;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AddressDTO {
    private String line;
    private String provinceCode;
    private String districtCode;
    private String wardCode;
}

