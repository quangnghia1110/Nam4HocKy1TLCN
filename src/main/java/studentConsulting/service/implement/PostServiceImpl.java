package studentConsulting.service.implement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cloudinary.Cloudinary;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.news.PostEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.PostDTO;
import studentConsulting.model.payload.request.news.CreatePostRequest;
import studentConsulting.model.payload.request.news.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.CommentRepository;
import studentConsulting.repository.PostRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IPostService;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class PostServiceImpl implements IPostService {

    @Autowired
    private PostRepository postRepository;

    @Autowired
    private CommentRepository commentRepository;

    @Autowired
	private Cloudinary cloudinary;
    
    @Autowired
    private UserRepository userRepository;

    
    @Override
    public DataResponse<PostDTO> createPost(CreatePostRequest postRequest, Integer userId) {
        String fileName = null;

        if (postRequest.getFile() != null && !postRequest.getFile().isEmpty()) {
            fileName = saveFile(postRequest.getFile());
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
                .createdAt(LocalDateTime.now())
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
    public DataResponse<PostDTO> updatePost(Integer id, UpdatePostRequest postRequest, Integer userId) {
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
            String fileName = saveFile(postRequest.getFile());
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

    private String saveFile(MultipartFile file) {
        try {
            String fileType = file.getContentType();

            if (fileType != null && fileType.startsWith("image")) {
                Map<String, Object> uploadResult = cloudinary.uploader().upload(file.getBytes(), Map.of());
                return (String) uploadResult.get("url");
            } else {
                String uploadDir = "D:\\Uploads";
                Path uploadPath = Paths.get(uploadDir);

                if (!Files.exists(uploadPath)) {
                    Files.createDirectories(uploadPath);
                }

                String originalFileName = file.getOriginalFilename();
                Path filePath = uploadPath.resolve(originalFileName);
                Files.write(filePath, file.getBytes());

                return filePath.toString();
            }
        } catch (IOException e) {
            throw new RuntimeException("Không thể lưu tệp. Lỗi: " + e.getMessage());
        }
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
