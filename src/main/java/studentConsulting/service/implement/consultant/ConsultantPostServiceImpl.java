package studentConsulting.service.implement.consultant;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.content.PostEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.content.PostDTO;
import studentConsulting.model.payload.request.content.CreatePostRequest;
import studentConsulting.model.payload.request.content.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.content.PostRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.repository.content.CommentRepository;
import studentConsulting.service.implement.common.CommonFileStorageServiceImpl;
import studentConsulting.service.interfaces.consultant.IConsultantPostService;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class ConsultantPostServiceImpl implements IConsultantPostService {

    @Autowired
    private PostRepository postRepository;

    @Autowired
    private CommentRepository commentRepository;

    @Autowired
    private Cloudinary cloudinary;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private CommonFileStorageServiceImpl fileStorageService;

    @Override
    public DataResponse<PostDTO> createPost(CreatePostRequest postRequest, Integer userId) {
        String fileName = null;

        if (postRequest.getFile() != null && !postRequest.getFile().isEmpty()) {
            fileName = fileStorageService.saveFile(postRequest.getFile());
        }

        Optional<UserInformationEntity> userOpt = userRepository.findById(userId);
        if (userOpt.isEmpty()) {
            throw new RuntimeException("Người dùng không tồn tại.");
        }

        UserInformationEntity user = userOpt.get();

        PostEntity post = PostEntity.builder()
                .content(postRequest.getContent())
                .isAnonymous(postRequest.isAnonymous())
                .fileName(fileName)
                .user(user)
                .createdAt(LocalDate.now())
                .isApproved(false)
                .views(0)
                .build();

        PostEntity savedPost = postRepository.save(post);

        PostDTO savedPostDTO = mapEntityToDTO(savedPost);

        return DataResponse.<PostDTO>builder()
                .status("success")
                .message("Bài đăng đã được tạo thành công")
                .data(savedPostDTO)
                .build();
    }


    @Override
    public DataResponse<PostDTO> updatePost(Integer id, UpdatePostRequest postRequest) {
        Optional<PostEntity> postOptional = postRepository.findById(id);

        if (postOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy bài viết.");
        }

        PostEntity post = postOptional.get();

        if (!post.isApproved()) {
            throw new ErrorException("Bài viết chưa được duyệt và không thể cập nhật.");
        }

        post.setContent(postRequest.getContent());
        post.setAnonymous(postRequest.isAnonymous());

        if (postRequest.getFile() != null && !postRequest.getFile().isEmpty()) {
            String fileName = fileStorageService.saveFile(postRequest.getFile());
            post.setFileName(fileName);
        }

        postRepository.save(post);
        PostDTO postDTO = mapEntityToDTO(post);

        return DataResponse.<PostDTO>builder()
                .status("success")
                .message("Bài viết đã được cập nhật thành công")
                .data(postDTO)
                .build();
    }


    @Override
    public DataResponse<String> deletePost(Integer id, Integer userId) {
        Optional<PostEntity> postOptional = postRepository.findById(id);

        if (postOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy bài viết.");
        }

        PostEntity post = postOptional.get();

        if (!post.getUser().getId().equals(userId.toString())) {
            throw new ErrorException("Bạn không có quyền xóa bài viết này.");
        }

        if (!post.isApproved()) {
            throw new ErrorException("Bài viết chưa được duyệt và không thể xóa.");
        }

        commentRepository.deleteByPost(post);

        postRepository.deleteById(post.getId());

        return DataResponse.<String>builder()
                .status("success")
                .message("Bài viết và các bình luận liên quan đã được xóa thành công")
                .build();
    }


    @Override
    public DataResponse<List<PostDTO>> getPendingPostsByUser(String userId) {
        List<PostEntity> pendingPosts = postRepository.findByIsApprovedFalseAndUser_Id(Integer.parseInt(userId));
        List<PostDTO> postDTOs = pendingPosts.stream()
                .map(this::mapEntityToDTO)
                .collect(Collectors.toList());

        return DataResponse.<List<PostDTO>>builder()
                .status("success")
                .message("Danh sách các bài đăng chờ duyệt của người dùng")
                .data(postDTOs)
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
