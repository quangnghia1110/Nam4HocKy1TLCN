package studentConsulting.model.payload.dto.address;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ManageAddressDTO {
    private Integer id;
    private String line;
    private String provinceCode;
    private String districtCode;
    private String wardCode;
}

