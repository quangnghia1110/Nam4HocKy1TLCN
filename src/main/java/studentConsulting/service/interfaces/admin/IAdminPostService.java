package studentConsulting.service.interfaces.admin;

import studentConsulting.model.payload.dto.content.PostDTO;
import studentConsulting.model.payload.response.DataResponse;

public interface IAdminPostService {
    DataResponse<PostDTO> approvePost(Integer postId);
}