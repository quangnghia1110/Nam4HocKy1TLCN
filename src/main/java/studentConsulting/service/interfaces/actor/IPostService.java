package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.actor.PostDTO;
import studentConsulting.model.payload.request.CreatePostRequest;
import studentConsulting.model.payload.request.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

public interface IPostService {
    DataResponse<PostDTO> createPost(CreatePostRequest postRequest, Integer userId);

    Page<PostDTO> getAllPostsWithFilters(boolean isApproved, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable);


    public Page<PostDTO> getPostByRole(boolean isApproved, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable, Principal principal);

    DataResponse<PostDTO> updatePost(Integer id, UpdatePostRequest postRequest, Integer userId);

    DataResponse<String> deletePost(Integer id, Integer userId);

    DataResponse<PostDTO> getPostById(Integer id, Integer userId);
    public Page<PostDTO> getAllPost(Pageable pageable);

}
