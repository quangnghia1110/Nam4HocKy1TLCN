package studentConsulting.model.entity.news;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "comment")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Comment {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id_comment")  
    private Integer idComment;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "id_post", nullable = false)
    private PostEntity post;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "id_user_comment", nullable = false)
    @JsonIgnore
    private UserInformationEntity userComment;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "id_comment_father")
    private Comment parentComment;

    @Column(nullable = false)
    private String comment;

    @Column(nullable = false)
    private LocalDateTime createDate;

    @OneToMany(mappedBy = "parentComment", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private List<Comment> childComments;
    
    public Comment(Integer idComment) {
        this.idComment = idComment;
    }
}

