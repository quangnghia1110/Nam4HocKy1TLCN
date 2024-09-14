package studentConsulting.service;

import java.util.List;

import studentConsulting.model.payload.dto.PostDTO;
import studentConsulting.model.payload.request.news.CreatePostRequest;
import studentConsulting.model.payload.response.DataResponse;

public interface IPostService {
	DataResponse<PostDTO> createPost(CreatePostRequest postRequest, Integer userId);
    DataResponse<List<PostDTO>> getPendingPostsByUser(String userId);
    DataResponse<PostDTO> approvePost(Long postId);
}
