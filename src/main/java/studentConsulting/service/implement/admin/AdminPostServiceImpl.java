package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.PostEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.PostDTO;
import studentConsulting.model.payload.mapper.admin.PostMapper;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.actor.PostRepository;
import studentConsulting.service.interfaces.admin.IAdminPostService;

@Service
public class AdminPostServiceImpl implements IAdminPostService {

    @Autowired
    private PostRepository postRepository;

    @Autowired
    private PostMapper postMapper;

    @Override
    public DataResponse<PostDTO> approvePost(Integer postId) {
        PostEntity post = postRepository.findById(postId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy bài viết."));
        post.setApproved(true);
        postRepository.save(post);

        PostDTO approvedPostDTO = postMapper.mapToDTO(post);
        return DataResponse.<PostDTO>builder()
                .status("success")
                .message("Bài viết đã được phê duyệt")
                .data(approvedPostDTO)
                .build();
    }
}
