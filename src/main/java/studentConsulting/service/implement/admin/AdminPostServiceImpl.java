package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.content.PostEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.PostDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.PostRepository;
import studentConsulting.service.interfaces.admin.IAdminPostService;

@Service
public class AdminPostServiceImpl implements IAdminPostService {

    @Autowired
    private PostRepository postRepository;

    @Override
    public DataResponse<PostDTO> approvePost(Integer postId) {
        PostEntity post = postRepository.findById(postId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy bài viết."));
        post.setApproved(true);
        postRepository.save(post);

        PostDTO approvedPostDTO = mapEntityToDTO(post);
        return DataResponse.<PostDTO>builder()
                .status("success")
                .message("Bài viết đã được phê duyệt")
                .data(approvedPostDTO)
                .build();
    }

    private PostDTO mapEntityToDTO(PostEntity postEntity) {
        return PostDTO.builder()
                .content(postEntity.getContent())
                .isAnonymous(postEntity.isAnonymous())
                .userId(postEntity.getUser().getId())
                .fileName(postEntity.getFileName())
                .createdAt(postEntity.getCreatedAt())
                .isApproved(postEntity.isApproved())
                .views(postEntity.getViews())
                .build();
    }
}
