package studentConsulting.service.interfaces.consultant;

import studentConsulting.model.payload.dto.content.PostDTO;
import studentConsulting.model.payload.request.content.CreatePostRequest;
import studentConsulting.model.payload.request.content.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;

import java.util.List;

public interface IConsultantPostService {
    DataResponse<PostDTO> createPost(CreatePostRequest postRequest, Integer userId);

    DataResponse<List<PostDTO>> getPendingPostsByUser(String userId);

    DataResponse<PostDTO> updatePost(Integer id, UpdatePostRequest postRequest);

    DataResponse<String> deletePost(Integer id, Integer userId);
}
