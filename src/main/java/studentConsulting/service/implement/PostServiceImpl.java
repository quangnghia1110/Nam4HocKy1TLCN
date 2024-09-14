package studentConsulting.service.implement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cloudinary.Cloudinary;

import studentConsulting.model.entity.news.PostEntity;
import studentConsulting.model.payload.dto.PostDTO;
import studentConsulting.model.payload.request.news.CreatePostRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.PostRepository;
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
public class PostServiceImpl implements IPostService{

    @Autowired
    private PostRepository postRepository;

    @Autowired
	private Cloudinary cloudinary;
    
    public DataResponse<PostDTO> createPost(CreatePostRequest postRequest, Integer userId) {
        String fileName = null;
        if (postRequest.getFile() != null && !postRequest.getFile().isEmpty()) {
            fileName = saveFile(postRequest.getFile());
        }

        PostDTO postDTO = mapRequestToDTO(postRequest, fileName);
        PostEntity post = mapDTOToEntity(postDTO, userId);
        post.setApproved(false);
        post.setViews(0);

        PostEntity savedPost = postRepository.save(post);
        postRepository.save(savedPost);

        PostDTO savedPostDTO = mapEntityToDTO(savedPost);

        return DataResponse.<PostDTO>builder()
                .status("success")
                .message("Bài đăng đã được tạo")
                .data(savedPostDTO)
                .build();
    }

    public DataResponse<List<PostDTO>> getPendingPostsByUser(String userId) {
        List<PostEntity> pendingPosts = postRepository.findByIsApprovedAndAuthor(false, userId);
        List<PostDTO> postDTOs = pendingPosts.stream()
                .map(this::mapEntityToDTO)
                .collect(Collectors.toList());

        return DataResponse.<List<PostDTO>>builder()
                .status("success")
                .message("Danh sách các bài đăng chờ duyệt của người dùng")
                .data(postDTOs)
                .build();
    }
    public DataResponse<PostDTO> approvePost(Long postId) {
        PostEntity post = postRepository.findById(postId)
                .orElseThrow(() -> new RuntimeException("Bài đăng không tìm thấy"));

        post.setApproved(true);
        postRepository.save(post);

        PostDTO approvedPostDTO = mapEntityToDTO(post);
        return DataResponse.<PostDTO>builder()
                .status("success")
                .message("Bài đăng đã được phê duyệt")
                .data(approvedPostDTO)
                .build();
    }

    private String saveFile(MultipartFile file) {
        try {
            String fileType = file.getContentType();

            if (fileType != null && fileType.startsWith("image")) {
                Map<String, Object> uploadResult = cloudinary.uploader().upload(file.getBytes(), Map.of());
                String fileUrl = (String) uploadResult.get("url");
                return fileUrl;
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

    private PostDTO mapRequestToDTO(CreatePostRequest postRequest, String fileName) {
        return PostDTO.builder()
                .content(postRequest.getContent())
                .isAnonymous(postRequest.isAnonymous())
                .fileName(fileName)
                .build();
    }

    private PostEntity mapDTOToEntity(PostDTO postDTO, Integer userId) {
        return PostEntity.builder()
                .content(postDTO.getContent())
                .isAnonymous(postDTO.isAnonymous())
                .author(userId.toString())
                .fileName(postDTO.getFileName())
                .createdAt(LocalDateTime.now())
                .build();
    }

    private PostDTO mapEntityToDTO(PostEntity postEntity) {
        return PostDTO.builder()
                .content(postEntity.getContent())
                .isAnonymous(postEntity.isAnonymous())
                .author(postEntity.getAuthor())
                .fileName(postEntity.getFileName())
                .createdAt(postEntity.getCreatedAt())
                .isApproved(postEntity.isApproved())
                .views(postEntity.getViews())
                .build();
    }
}

