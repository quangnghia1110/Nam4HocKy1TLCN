package studentConsulting.model.payload.request.address;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class WardRequest {
    private String code;       
    private String fullName;
    private String districtCode; 
}

