package studentConsulting.model.payload.request.consultant;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
@Setter
@Getter
public class ApproveMemberRequest {
    private List<String> emailToApprove;
}

