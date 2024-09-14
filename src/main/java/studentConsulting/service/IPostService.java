package studentConsulting.service;

import java.util.List;

import studentConsulting.model.payload.dto.PostDTO;
import studentConsulting.model.payload.request.news.CreatePostRequest;
import studentConsulting.model.payload.request.news.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;

public interface IPostService {
	DataResponse<PostDTO> createPost(CreatePostRequest postRequest, Integer userId);
    DataResponse<List<PostDTO>> getPendingPostsByUser(String userId);
    DataResponse<PostDTO> approvePost(Integer postId);
    public DataResponse<PostDTO> updatePost(Integer id, UpdatePostRequest postRequest, Integer userId);
    public DataResponse<String> deletePost(Integer id, Integer userId);
}
